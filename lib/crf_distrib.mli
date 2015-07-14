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

type 'a t = (float * 'a) array

val normalize : 'a t -> 'a t

(** [choose ~rng [(p1, x1); (p2, x2); ...; (pn, xn)]] chooses [xi]
    with probability [pi]. Each probability must be normalized. *)
val choose : rng:Gsl.Rng.t -> 'a t -> 'a

(** [local_trans model w in_vertex out_vertex] makes (unnormalized) probability
    distribution of transition from the current label of [out_vertex] to other
    output labels. This function destructively modifies [out_vertex]. *)
val local_trans :
  ('a, 'b, 'kv, 'ke) Crf_model.model ->
  ('kv, 'ke) Crf_model.fwvec ->
  ('a, 'b) Crf_model.in_graph ->
  'b Crf_model.graph -> 'b t
