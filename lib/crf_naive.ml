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

(** Naive implementation of functions for CRF *)

open Format
open Slap.D
open Crf_model

module G = Crf_graph

let fold ~all { out_labels = cands; _ } f init ig =
  let og = create_out_graph ~all cands ig in
  let rec aux acc = function
    | [] -> f acc og
    | hd :: tl ->
      Array.fold_left (fun acc' candi -> G.set hd candi ; aux acc' tl) acc cands
  in
  let asgn = if all
    then G.fold (fun acc ov -> ov :: acc) [] og
    else G.fold2
        (fun acc iv ov ->
           if (G.get iv).out_label = None then ov :: acc else acc)
        [] ig og in
  let acc = aux init asgn in
  acc

let normalizer model w ig =
  let z = fold ~all:true model
      (fun acc og -> acc +. graph_potential model w ig og)
      0.0 ig in
  1.0 /. z

let prob model w ig og =
  normalizer model w ig *. graph_potential model w ig og

let mean_feature_vec ~all model w ig =
  let ev = Vec.make0 (fwsize model) in
  let add_to_ev acc og =
    let x = graph_feature model ig og in
    let pot = exp (dot w x) in
    axpy ~alpha:pot x ev;
    acc +. pot
  in
  let sum = fold ~all model add_to_ev 0. ig in
  scal (1. /. sum) ev;
  ev

let log_likelihood model w ig =
  fold ~all:false model
    (fun acc og -> acc +. graph_potential model w ig og)
    0.0 ig
  |> ( *. ) (normalizer model w ig)
  |> log

let gradient ?(epsilon = 1e-4) w f =
  let eps_vec x i eps = Vec.mapi (fun j xj -> if i=j then xj+.eps else xj) x in
  Vec.init (Vec.dim w)
    (fun i ->
       let pp = f (eps_vec w i (~+. epsilon)) in
       let pn = f (eps_vec w i (~-. epsilon)) in
       (pp -. pn) /. (2.0 *. epsilon))

let grad_log_likelihood ?epsilon model w ig =
  gradient ?epsilon w (fun w' -> log_likelihood model w' ig)

let infer ~all model w ig =
  let val_of = function
    | None -> assert(false)
    | Some x -> x
  in
  let aux acc og =
    let pot = graph_potential model w ig og in
    match acc with
    | None -> Some (pot, G.copy og)
    | Some (max_pot, _) ->
      if max_pot > pot then acc else Some (pot, G.copy og)
  in
  match fold ~all model aux None ig with
  | Some (_, og) -> og
  | None -> G.map (fun ov -> val_of (G.get ov).out_label) ig
