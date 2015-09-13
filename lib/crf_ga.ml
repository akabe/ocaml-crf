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

module G = Crf_graph
module M = Crf_model

type t =
  {
    ga_generation : int; (** # of loops *)
    ga_elite : int; (** # of individuals that remans in the next generation by
                        elite selection *)
    ga_unico : int; (** # of pairs of individuals generated by uniform
                        crossover *)
    ga_2pco : int; (** # of pairs of individuals generated by two point
                       crossover *)
    ga_mutate : float; (** probability of mutation *)
  }

let default = { ga_generation = 100;
                ga_elite = 10;
                ga_unico = 3;
                ga_2pco = 3;
                ga_mutate = 0.05; }

let ( >> ) f g x = g (f x)

let prob rng p = Gsl.Rng.uniform rng < p

module Array = struct
  include Array

  let findi f arr =
    let n = Array.length arr in
    let rec aux i =
      if i = n then raise Not_found
      else if f arr.(i) then i else aux (i + 1)
    in
    aux 0

  let replace_sub ~from ~len f arr =
    for i = 0 to len - 1 do
      arr.(from + i) <- f i
    done

  let replace_all f arr = replace_sub ~from:0 ~len:(Array.length arr) f arr

  let pair_replace_sub ~from ~len f arr =
    for i = 0 to len - 1 do
      let (x, y) = f i in
      let j = 2 * i + from in
      arr.(j) <- x ; arr.(j + 1) <- y
    done

  let find_sub ~from ~len f arr =
    try
      for i=from to from+len-1 do if f arr.(i) then raise Pervasives.Exit done;
      false
    with Pervasives.Exit -> true

  let max_map f arr =
    let n = Array.length arr in
    let rec aux i max_x max_y =
      if i = n then (max_x, max_y) else begin
        let y = f arr.(i) in
        if y > max_y then aux (i+1) arr.(i) y else aux (i+1) max_x max_y
      end
    in
    if n = 0 then invalid_arg "Array.max_map" else aux 1 arr.(0) (f arr.(0))
end

let gene model og =
  let aux ov =
    model.M.out_labels
    |> Array.findi ((=) (G.get ov))
    |> string_of_int
  in
  G.fold (fun acc ov -> acc ^ aux ov) "" og

let make_sorted_distrib model w ig ogs =
  let score og = M.graph_potential model w ig og |> log in
  let distrib = ogs
                |> Array.map (fun og -> (score og, og))
                |> Crf_distrib.normalize in
  Array.fast_sort (fun (p1, _) (p2, _) -> ~- (compare p1 p2)) distrib;
  distrib

let make_crossover f ~rng distrib =
  let make_pair ov1 ov2 =
    let x = G.get ov1 in
    let y = G.get ov2 in
    if f () then (x, y) else (y, x)
  in
  let og1 = Crf_distrib.choose ~rng distrib in
  let og2 = Crf_distrib.choose ~rng distrib in
  let g = G.map2 make_pair og1 og2 in
  (G.map (G.get >> fst) g, G.map (G.get >> snd) g)

let uniform_crossover ~rng =
  make_crossover (fun () -> Gsl.Rng.uniform_int rng 2 = 0) ~rng

let two_point_crossover ~rng gsize =
  let aux i j =
    let c = ref (-1) in
    make_crossover (fun () -> incr c ; !c >= i && !c < j) ~rng
  in
  let m = Gsl.Rng.uniform_int rng gsize in
  let n = Gsl.Rng.uniform_int rng gsize in
  if m < n then aux m n else aux n m

let mutate ~rng ~all p model ig og =
  let n = Array.length model.M.out_labels in
  let rec genlabel x =
    let y = model.M.out_labels.(Gsl.Rng.uniform_int rng n) in
    if x <> y then y else genlabel x
  in
  let mutate_all _ ov =
    let x = G.get ov in
    if prob rng p then genlabel x else x
  in
  let mutate_none iv ov =
    if (G.get iv).M.out_label = None then mutate_all iv ov else G.get ov
  in
  G.map2 (if all then mutate_all else mutate_none) ig og

let infer ~rng ~all ga model w ig =
  let gsize = G.size ig in
  let ogs = Array.init
      (ga.ga_elite + (ga.ga_unico + ga.ga_2pco) * 2)
      (fun i ->
         M.random_out_graph ~all:(if i < ga.ga_elite then false else all)
           ~rng model.M.out_labels ig) in
  let cur_best distrib = (* the best solution in the current generation *)
    let (_, og) = distrib.(0) in
    (og, M.graph_potential model w ig og)
  in
  let rec loop n best_og best_pot distrib =
    if n = ga.ga_generation then best_og
    else begin
      Array.replace_sub (* Copy elite genes *)
        (fun i -> snd distrib.(i))
        ~from:0 ~len:ga.ga_elite ogs;
      Array.pair_replace_sub (* Perform uniform crossover *)
        (fun _ -> uniform_crossover ~rng distrib)
        ~from:ga.ga_elite ~len:ga.ga_unico ogs;
      Array.pair_replace_sub (* Perform two point crossover *)
        (fun _ -> two_point_crossover ~rng gsize distrib)
        ~from:(ga.ga_elite + ga.ga_unico * 2) ~len:ga.ga_2pco ogs;
      Array.replace_all (* Execute mutation *)
        (fun i -> mutate ~rng ~all ga.ga_mutate model ig ogs.(i)) ogs;
      let distrib' = make_sorted_distrib model w ig ogs in
      let (og, pot) = cur_best distrib' in
      if pot > best_pot then loop (n+1) og pot distrib'
      else loop (n+1) best_og best_pot distrib'
    end
  in
  let distrib = make_sorted_distrib model w ig ogs in
  let (og, pot) = cur_best distrib in
  loop 0 og pot distrib
