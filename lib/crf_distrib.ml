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

open Format
open Slap.D
open Crf_model

type 'a t = (float * 'a) array

let normalize dist =
  let s = Array.fold_left (fun acc (p, _) -> acc +. p) 0.0 dist in
  let r = 1.0 /. s in
  Array.map (fun (p, x) -> (p *. r, x)) dist

let choose ~rng dist =
  let n = Array.length dist in
  if n = 0 then failwith "empty distribution";
  let rec loop z i =
    let (p, x) = dist.(i) in
    if i = 0 || p > z then x else loop (z -. p) (i - 1)
  in
  loop (Gsl.Rng.uniform rng) (n - 1)

let local_trans model w iv ov =
  model.out_labels
  |> Array.map (fun ol ->
      Crf_graph.set ov ol;
      (local_potential model w iv ov, ol))
