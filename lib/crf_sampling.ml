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

module G = Crf_graph

(** [sampling1 ~rng ~all model w in_graph out_graph] destructively updates all
    output labels in [out_graph] by Gibbs sampling. *)
let sampling1 ~rng ~all model w ig og =
  let update iv ov =
    Crf_distrib.local_trans model w iv ov
    |> Crf_distrib.normalize
    |> Crf_distrib.choose ~rng
    |> G.set ov
  in
  let update_none iv ov =
    if (G.get iv).lb_output = None then update iv ov
  in
  G.iter2 (if all then update else update_none) ig og

let fold ~rng ~all smp model w f init ig =
  let acc = ref init in
  for _ = 1 to smp.sequences do
    let og = random_out_graph ~rng ~all model.out_labels ig in
    for _ = 1 to smp.burn_in do sampling1 ~rng ~all model w ig og done;
    for _ = 1 to smp.samples do
      for _ = 1 to smp.period do sampling1 ~rng ~all model w ig og done;
      acc := f !acc og
    done
  done;
  !acc

let normalizer ~rng smp model w ig =
  let n = float (Array.length model.out_labels) ** float (G.size ig) in
  let z' = fold ~rng ~all:true smp model w
      (fun acc og -> acc +. 1. /. graph_potential model w ig og)
      0.0 ig in
  z' /. (float (smp.samples * smp.sequences) *. n)

let prob ~rng smp model w ig og =
  normalizer ~rng smp model w ig *. graph_potential model w ig og

let mean_feature_vec ~rng ~all smp model w ig =
  let ev = Vec.make0 (fwsize model) in
  let alpha = 1.0 /. float (smp.samples * smp.sequences) in
  let add_to_ev () og = axpy ~alpha (graph_feature model ig og) ev in
  fold ~rng ~all smp model w add_to_ev () ig;
  ev

let grad_log_likelihood ~rng smp model w ig =
  let dJ_dw = mean_feature_vec ~rng ~all:false smp model w ig in
  axpy ~alpha:(-1.) (mean_feature_vec ~rng ~all:true smp model w ig) dJ_dw;
  dJ_dw
