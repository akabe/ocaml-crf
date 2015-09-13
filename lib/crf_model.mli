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

(** Commonly used typed and functions *)

open Slap.D

(** The type of labels of input graphs. *)
type ('a, 'b) label =
  {
    in_label : 'a;
    out_label : 'b option;
  }

(** The type of graphs. *)
type 'a graph = 'a Crf_graph.t

(** The type of input graphs. *)
type ('a, 'b) in_graph = ('a, 'b) label graph

(** The type of models. *)
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

(** {2 Construction of output graphs} *)

val create_out_graph :
  all:bool -> 'b ->
  ('a, 'b) in_graph -> 'b graph

val random_out_graph :
  rng:Gsl.Rng.t ->
  all:bool ->
  'b array ->
  ('a, 'b) in_graph -> 'b graph

(** {2 Feature/weight vectors} *)

type ('kv, 'ke) fwsize = ('kv, 'ke) Slap.Size.add
type ('kv, 'ke) fwvec = (('kv, 'ke) fwsize, Slap.cnt) vec

val fwsize : ('a, 'b, 'kv, 'ke) model -> ('kv, 'ke) fwsize Slap.Size.t

val split_fwvec :
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec -> ('kv, _) vec * ('ke, _) vec

val random_fwvec :
  ?range:float ->
  ?from:float ->
  rng:Gsl.Rng.t ->
  ('a, 'b, 'kv, 'ke) model -> ('kv, 'ke) fwvec

val graph_feature :
  ('a, 'b, 'kv, 'ke) model ->
  ('a, 'b) in_graph ->
  'b graph -> ('kv, 'ke) fwvec

(** {2 Potential} *)

val local_potential :
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec ->
  ('a, 'b) in_graph ->
  'b graph -> float

val graph_potential :
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec ->
  ('a, 'b) in_graph ->
  'b graph -> float
