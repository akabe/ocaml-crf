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

module Graph = Crf_graph

include Crf_model

module Train = Crf_train

module Eval = Crf_eval

module Naive = Crf_naive

module Sampling = Crf_sampling

(** Simulated annealing *)
module SA = Crf_sa

module GA = Crf_ga

module PfGA = Crf_pfga
