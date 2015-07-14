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

(** Simulated annealing *)

val infer :
  rng:Gsl.Rng.t ->
  all:bool ->
  Crf_model.sa ->
  ('a, 'b, 'kv, 'ke) Crf_model.model ->
  ('kv, 'ke) Crf_model.fwvec ->
  ('a, 'b) Crf_model.in_graph -> 'b Crf_model.graph
