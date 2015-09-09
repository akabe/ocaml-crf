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

(** Graph structure. *)

open Format

(** The type of vertices. *)
type 'a t

(** {2 Basic operation} *)

(** Create a vertex that has a given label. *)
val create : 'a -> 'a t

(** Copy a graph. *)
val copy : 'a t -> 'a t

(** [connect p c] connects two vertices [p] (as a parent) and [c] (as a child).
*)
val connect : 'a t -> 'a t -> unit

(** [disconnect p c] disconnects two vertices [p] (as a parent) and [c] (as a
    child). *)
val disconnect : 'a t -> 'a t -> unit

(** Return the label of a given vertex. *)
val get : 'a t -> 'a

(** Set the label to a given vertex. *)
val set : 'a t -> 'a -> unit

(** Return the list of parents of a given vertex. *)
val parents : 'a t -> 'a t list

(** Return the list of children of a given vertex. *)
val children : 'a t -> 'a t list

(** [neighbors v] returns [parent v :: children v] (if [v] has a parent) or
    [children v] (if [v] is the root). *)
val neighbors : 'a t -> 'a t list

(** [edges v] returns the list of directed edges connected to [v]. Each edge is
    a pair of [(parent, child)]. *)
val edges : 'a t -> ('a t * 'a t) list

(** Count the number of vertices in a given graph. *)
val size : 'a t -> int

(** Check whether a given graph is well-formed, or not. *)
val validate : 'a t -> bool

(** {2 Iterators}

    The following functions touches vertices in preorder. *)

val map : ('a t -> 'b) -> 'a t -> 'b t

val map2 : ('a t -> 'b t -> 'c) -> 'a t -> 'b t -> 'c t

val fold : ('accum -> 'a t -> 'accum) -> 'accum -> 'a t -> 'accum

val fold2 :
  ('accum -> 'a t -> 'b t -> 'accum) -> 'accum -> 'a t -> 'b t -> 'accum

val iter : ('a t -> unit) -> 'a t -> unit

val iter2 : ('a t -> 'b t -> unit) -> 'a t -> 'b t -> unit

(** {2 Comparison} *)

val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> bool

(** {2 Pretty printing} *)

val pp_graph : (formatter -> 'a t -> unit) -> formatter -> 'a t -> unit

val output_image : (formatter -> 'a t -> unit) -> string -> 'a t -> unit
