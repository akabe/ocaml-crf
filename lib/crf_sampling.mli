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

(** Gibbs sampling *)

type t =
  {
    sequences : int;
    samples : int;
    period : int;
    burn_in : int;
  }

val default : t

val normalizer :
  rng:Gsl.Rng.t -> t ->
  ('a, 'b, 'kv, 'ke) Crf_model.model ->
  ('kv, 'ke) Crf_model.fwvec ->
  ('a, 'b) Crf_model.in_graph -> float

val prob :
  rng:Gsl.Rng.t -> t ->
  ('a, 'b, 'kv, 'ke) Crf_model.model ->
  ('kv, 'ke) Crf_model.fwvec ->
  ('a, 'b) Crf_model.in_graph ->
  'b Crf_model.graph -> float

val mean_feature_vec :
  rng:Gsl.Rng.t -> all:bool -> t ->
  ('a, 'b, 'kv, 'ke) Crf_model.model ->
  ('kv, 'ke) Crf_model.fwvec ->
  ('a, 'b) Crf_model.in_graph -> ('kv, 'ke) Crf_model.fwvec

val grad_log_likelihood :
  rng:Gsl.Rng.t -> t ->
  ('a, 'b, 'kv, 'ke) Crf_model.model ->
  ('kv, 'ke) Crf_model.fwvec ->
  ('a, 'b) Crf_model.in_graph -> ('kv, 'ke) Crf_model.fwvec
