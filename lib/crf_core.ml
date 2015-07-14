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

let pow =
  let rec aux acc x n =
    if n = 0 then acc
    else if n land 1 = 0 then aux acc (x * x) (n lsr 1)
    else aux (acc * x) x (n - 1)
  in
  aux 1

let select smp model ig f_naive f_smpl =
  (* Evaluate computation costs by the number of loops. *)
  let gsize = Crf_graph.size ig in
  let cost1 = pow (Array.length model.out_labels) gsize in
  let cost2 = (smp.samples * smp.period + smp.burn_in)
              * smp.sequences * gsize in
  if cost1 < cost2 then f_naive () else f_smpl ()

let prob ~rng smp model w ig og =
  select smp model ig
    (fun () -> Crf_naive.prob model w ig og)
    (fun () -> Crf_sampling.prob ~rng smp model w ig og)

let normalizer ~rng smp model w ig =
  select smp model ig
    (fun () -> Crf_naive.normalizer model w ig)
    (fun () -> Crf_sampling.normalizer ~rng smp model w ig)

let mean_feature_vec ~rng ~all smp model w ig =
  select smp model ig
    (fun () -> Crf_naive.mean_feature_vec ~all model w ig)
    (fun () -> Crf_sampling.mean_feature_vec ~rng ~all smp model w ig)

let grad_log_likelihood ~rng smp model w igs =
  let dJ_dw = Vec.make0 (fwsize model) in
  List.iter
    (fun ig ->
       axpy (mean_feature_vec ~rng ~all:false smp model w ig) dJ_dw;
       axpy ~alpha:(-1.) (mean_feature_vec ~rng ~all:true smp model w ig) dJ_dw)
    igs;
  dJ_dw

let grad_log_posterior ~rng ~sigma2 smp model w igs =
  let dJ_dw = grad_log_likelihood ~rng smp model w igs in
  axpy ~alpha:((-1.) /. sigma2) w dJ_dw;
  dJ_dw

let infer ~rng ~all sa model w ig =
  let gsize = Crf_graph.size ig in
  let c_naive = pow (Array.length model.out_labels) gsize in
  let c_sa = sa.loops * gsize in
  if c_naive < c_sa then Crf_naive.infer ~all model w ig
  else Crf_sa.infer ~rng ~all sa model w ig
