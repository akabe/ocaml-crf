(* OCaml-CRF --- A library for conditional random field (CRF) in OCaml

   Copyright (C) 2015 Akinori ABE <abe@sf.ecei.tohoku.ac.jp>

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <http://www.gnu.org/licenses/>. *)

open Crf_model

module G = Crf_graph

let ( >> ) f g x = g (f x)

let rec compose f n x =
  if n <= 0 then x
  else if n land 1 = 0 then compose (f >> f) (n / 2) x
  else compose f (n - 1) (f x)

(** [unique_indices ~rng bound n] returns a list of unique integers of length
    [n]. [0 <= i < bound] for any elements [i] in a list. *)
let unique_indices ~rng bound n =
  let rec aux i ps =
    if i = 0 then ps else begin
      let p = Gsl.Rng.uniform_int rng bound in
      if List.mem p ps then aux i ps else aux (i - 1) (p :: ps)
    end
  in
  aux (n - 1) []

(** {2 Crossover} *)

let gen_crossover_points ~rng ~n =
  let k = Gsl.Rng.uniform_int rng (n - 2) + 2 in (* #points (2 <= k <= n-1) *)
  List.fast_sort compare (0 :: n :: unique_indices ~rng n (k - 1))

(** [multipoint_crossover ~rng ~n og1 og2] performs multi-point crossover. The
    number of crossover points and crossover positions are randomly selected.
    @param n = [Crf.Graph.size og1] = [Crf.Graph.size og2] *)
let multipoint_crossover ~rng ~n og1 og2 =
  let rec check i = function
    | [] | [_] -> false
    | bp :: ep :: ps -> (bp <= i && i < ep) || check i ps
  in
  let make_pair c ps ov1 ov2 =
    let x = G.get ov1 in
    let y = G.get ov2 in
    incr c ; if check !c ps then (x, y) else (y, x)
  in
  if n <= 1 then (og1, og2)
  else begin
    let ps = gen_crossover_points ~rng ~n in
    let g = G.map2 (make_pair (ref (-1)) ps) og1 og2 in
    (G.map (G.get >> fst) g, G.map (G.get >> snd) g)
  end

(** {2 Mutation} *)

(** [mutate ~rng ~n ga og1 og2] performs mutation. The number of mutation points
    and positions are randomly selected.
    @param n = [Crf.Graph.size ig] = [Crf.Graph.size og] *)
let mutate ~rng ~all ~n model ig og =
  let ps = unique_indices ~rng n (Gsl.Rng.uniform_int rng n + 1) in
  let k = Array.length model.out_labels in
  let rec genlabel x =
    let y = model.out_labels.(Gsl.Rng.uniform_int rng k) in
    if x <> y then y else genlabel x
  in
  let aux c iv ov =
    let x = G.get ov in
    incr c;
    if (all || (G.get iv).out_label = None) && List.mem !c ps
    then genlabel x else x
  in
  G.map2 (aux (ref (-1))) ig og

(** {2 Selection} *)

type 'a individual = { graph : 'a G.t; score : float; }

let select_global ~rng ~all model w ig =
  let og = random_out_graph ~rng ~all model.out_labels ig in
  { graph = og; score = graph_log_potential model w ig og; }

let select_local ~rng pool =
  let n = List.length pool in (* hold: n >= 2 *)
  let rec gen_j i =
    let j = Gsl.Rng.uniform_int rng n in
    if i < j then (i, j) else if i > j then (j, i) else gen_j i
  in
  let (i, j) = gen_j (Gsl.Rng.uniform_int rng n) in (* hold: i <> j && i < j *)
  let rec aux k (xi, xs) = function
    | [] -> assert false
    | h :: t ->
      if k = j then (xi, h, List.rev_append t xs)
      else if k = i then aux (k + 1) (h, xs) t
      else aux (k + 1) (xi, h :: xs) t
  in
  let dummy = List.hd pool in
  aux 0 (dummy, []) pool

let select_parents ~rng ~all model w ig pool =
  let (p1, p2, pool') = match pool with
    | [] -> assert false
    | [x] -> (x, select_global ~rng ~all model w ig, [])
    | _ -> select_local ~rng pool in
  if p1.score > p2.score then (p1, p2, pool') else (p2, p1, pool')

(** {2 Inference} *)

let make_children ~rng ~all ~n model w ig og1 og2 =
  let (og3, og4) = multipoint_crossover ~rng ~n og1 og2 in
  let (og3, og4) = if Gsl.Rng.uniform_int rng 2 = 1
    then (mutate ~rng ~all ~n model ig og3, og4)
    else (og3, mutate ~rng ~all ~n model ig og4) in
  let c3 = { graph = og3; score = graph_log_potential model w ig og3; } in
  let c4 = { graph = og4; score = graph_log_potential model w ig og4; } in
  if c3.score > c4.score then (c3, c4) else (c4, c3)

let infer ~rng ~all ~generation model w ig =
  let n = G.size ig in
  let max x y = if x.score > y.score then x else y in
  let update (best, pool) =
    let (p1, p2, pool') = select_parents ~rng ~all model w ig pool in
    let (c1, c2) = make_children ~rng ~all ~n model w ig p1.graph p2.graph in
    (* hold: p1.score > p2.score && c1.score > c2.score *)
    (* Case A: both of two children are better than both of two parents. *)
    if c2.score > p1.score then (max c1 best, p1 :: c1 :: c2 :: pool')
    (* Case B: both of two chilren are worse than both of two parents. *)
    else if p2.score > c1.score then (max p1 best, p1 :: pool')
    (* Case C: Either of two parents is better than both of two children. *)
    else if p1.score > c1.score then (max p1 best, p1 :: c1 :: pool')
    (* Case D: Either of two chilren is better than both of two parents. *)
    else begin
      let x = select_global ~rng ~all model w ig in
      (max (max c1 x) best, c1 :: x :: pool')
    end
  in
  let x = select_global ~rng ~all model w ig in
  let (best, _) = compose update generation (x, [x]) in
  best.graph
