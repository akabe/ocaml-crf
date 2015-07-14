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

val prob :
  rng:Gsl.Rng.t ->
  sampler ->
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec ->
  ('a, 'b) in_graph -> 'b graph -> float

val normalizer :
  rng:Gsl.Rng.t ->
  sampler ->
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec ->
  ('a, 'b) in_graph -> float

val mean_feature_vec :
  rng:Gsl.Rng.t ->
  all:bool ->
  sampler ->
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec ->
  ('a, 'b) in_graph -> ('kv, 'ke) fwvec

val grad_log_likelihood :
  rng:Gsl.Rng.t ->
  sampler ->
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec ->
  ('a, 'b) in_graph list -> ('kv, 'ke) fwvec

val grad_log_posterior :
  rng:Gsl.Rng.t ->
  sigma2:float ->
  sampler ->
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec ->
  ('a, 'b) in_graph list -> ('kv, 'ke) fwvec

val infer :
  rng:Gsl.Rng.t ->
  all:bool ->
  sa ->
  ('a, 'b, 'kv, 'ke) model ->
  ('kv, 'ke) fwvec ->
  ('a, 'b) in_graph -> 'b graph
