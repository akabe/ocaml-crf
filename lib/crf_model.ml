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

open Slap.D

type ('a, 'b) label =
  {
    lb_input : 'a;
    lb_output : 'b option;
  }

type 'a graph = 'a Crf_graph.t

type ('a, 'b) in_graph = ('a, 'b) label graph

type ('a, 'b, 'kv, 'ke) model =
  {
    out_labels : 'b array;
    vertex_dim : 'kv Slap.Size.t;
    edge_dim : 'ke Slap.Size.t;

    vertex_ff :
      ('a, 'b) in_graph -> 'b graph -> ('kv, Slap.cnt) vec;
    edge_ff :
      ('a, 'b) in_graph -> 'b graph ->
      ('a, 'b) in_graph -> 'b graph -> ('ke, Slap.cnt) vec;
  }

(** {2 Output graphs} *)

let create_out_graph ~all cands ig =
  let aux = if all
    then (fun _ -> cands.(0))
    else (fun v -> match (Crf_graph.get v).lb_output with
        | None -> cands.(0)
        | Some ol -> ol) in
  Crf_graph.map aux ig

let random_out_graph ~rng ~all cands ig =
  let n = Array.length cands in
  let gen_out_label () = cands.(Gsl.Rng.uniform_int rng n) in
  let aux = if all
    then (fun _ -> gen_out_label ())
    else (fun iv -> match (Crf_graph.get iv).lb_output with
        | None -> gen_out_label ()
        | Some ol -> ol) in
  Crf_graph.map aux ig

(** {2 Feature/weight vectors} *)

type ('kv, 'ke) fwsize = ('kv, 'ke) Slap.Size.add
type ('kv, 'ke) fwvec = (('kv, 'ke) fwsize, Slap.cnt) vec

let fwsize model = Slap.Size.add model.vertex_dim model.edge_dim

let split_fwvec model (fv : ('kv, 'ke) fwvec) : ('kv, _) vec * ('ke, _) vec =
  let nfv = Vec.subcntvec_dyn model.vertex_dim ~ofsx:1 fv in
  let ofsx = Slap.Size.to_int model.vertex_dim + 1 in
  let efv = Vec.subcntvec_dyn model.edge_dim ~ofsx fv in
  (nfv, efv)

let random_fwvec ?(range = 2.0) ?(from = -1.0) ~rng model =
  Vec.init (fwsize model) (fun _ -> range *. Gsl.Rng.uniform rng -. from)

let graph_feature model ig og =
  let f = Vec.make0 (fwsize model) in
  let (fv, fe) = split_fwvec model f in
  let add_feature iv ov =
    axpy (model.vertex_ff iv ov) fv;
    List.iter2
      (fun iv' ov' -> axpy (model.edge_ff iv ov iv' ov') fe)
      (Crf_graph.children iv) (Crf_graph.children ov)
  in
  Crf_graph.iter2 add_feature ig og;
  f

(** {2 Potential} *)

let map_sum2 f = List.fold_left2 (fun acc x y -> acc +. f x y) 0.0

let local_potential model w iv ov =
  let (wv, we) = split_fwvec model w in
  let pot_v = dot wv (model.vertex_ff iv ov) in
  let pot_e = map_sum2
      (fun (iv1, iv2) (ov1, ov2) -> dot we (model.edge_ff iv1 ov1 iv2 ov2))
      (Crf_graph.edges iv) (Crf_graph.edges ov) in
  exp (pot_v +. pot_e)

let graph_potential model w ig og =
  let (wv, we) = split_fwvec model w in
  let add_potential acc iv ov =
    let pot_v = dot wv (model.vertex_ff iv ov) in
    let pot_e = map_sum2
        (fun iv' ov' -> dot we (model.edge_ff iv ov iv' ov'))
        (Crf_graph.children iv) (Crf_graph.children ov) in
    acc +. pot_v +. pot_e
  in
  exp (Crf_graph.fold2 add_potential 0.0 ig og)

(** {2 Settings for Gibbs sampler} *)

type sampler =
  {
    sequences : int;
    samples : int;
    period : int;
    burn_in : int;
  }

let default_sampler =
  {
    sequences = 3;
    samples = 50;
    period = 1;
    burn_in = 100;
  }

(** {2 Settings for simulated annealing} *)

type sa =
  {
    loops : int;
    init_temp : float;
    decr_temp : float;
  }

let default_sa =
  {
    loops = 100;
    init_temp = 1000.;
    decr_temp = 0.9;
  }
