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

let choose_out_label ~rng ~temp model w iv ov =
  let dist =
    let pot0 = local_potential model w iv ov in
    Crf_distrib.local_trans model w iv ov
    |> Array.map (fun (pot, ol) -> (pot, (pot -. pot0, ol)))
    |> Crf_distrib.normalize
  in
  let rec aux () =
    let d, og = Crf_distrib.choose ~rng dist in
    if d > 0.0 then og
    else if exp (d /. temp) > Gsl.Rng.uniform rng then og
    else aux ()
  in
  aux ()

let update ~rng ~all ~temp model w ig og =
  let updv iv ov = G.set ov (choose_out_label ~rng ~temp model w iv ov) in
  let updv_none iv ov = if (G.get iv).out_label = None then updv iv ov in
  G.iter2 (if all then updv else updv_none) ig og

let infer ~rng ~all sa model w ig =
  let og = random_out_graph ~rng ~all model.out_labels ig in
  let temp = ref sa.init_temp in (* temperature *)
  for _ = 1 to sa.loops do
    update ~rng ~all ~temp:!temp model w ig og;
    temp := sa.decr_temp *. !temp
  done;
  og
