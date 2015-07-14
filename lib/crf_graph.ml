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

type 'a t =
  {
    mutable v_label : 'a;
    mutable v_parents : 'a t list;
    mutable v_children : 'a t list;
  }

module Ptr : sig
  type t = private int
  val compare : t -> t -> int
  val get : 'a -> t
end = struct
  type t = int
  let compare = Pervasives.compare
  let get x = Obj.magic x * 2
end

module PtrSet = Set.Make(Ptr)

(** {2 Iterators} *)

let connect_to_children v children =
  List.iter (fun c -> c.v_parents <- v :: c.v_parents) children;
  v.v_children <- children

let map f =
  let tbl = Hashtbl.create 16 in
  let rec aux v =
    try Hashtbl.find tbl (Ptr.get v)
    with Not_found ->
      let v' = { v_label = f v; v_parents = []; v_children = []; } in
      Hashtbl.add tbl (Ptr.get v) v';
      connect_to_children v' (List.map aux v.v_children);
      v'
  in
  aux

let map2 f =
  let tbl = Hashtbl.create 16 in
  let rec aux v1 v2 =
    try Hashtbl.find tbl (Ptr.get v1)
    with Not_found ->
      let v' = { v_label = f v1 v2; v_parents = []; v_children = []; } in
      Hashtbl.add tbl (Ptr.get v1) v';
      connect_to_children v' (List.map2 aux v1.v_children v2.v_children);
      v'
  in
  aux

let fold f =
  let seen = ref PtrSet.empty in
  let rec aux acc v =
    let p = Ptr.get v in
    if PtrSet.mem p !seen then acc else begin
      seen := PtrSet.add p !seen;
      List.fold_left aux (f acc v) v.v_children
    end
  in
  aux

let fold2 f =
  let seen = ref PtrSet.empty in
  let rec aux acc v1 v2 =
    let p = Ptr.get v1 in
    if PtrSet.mem p !seen then acc else begin
      seen := PtrSet.add p !seen;
      List.fold_left2 aux (f acc v1 v2) v1.v_children v2.v_children
    end
  in
  aux

let iter f = fold (fun () -> f) ()

let iter2 f = fold2 (fun () -> f) ()

(** {2 Basic operation} *)

let create v_label = { v_label; v_parents = []; v_children = []; }

let copy g = map (fun v -> v.v_label) g

let connect p c =
  p.v_children <- c :: p.v_children;
  c.v_parents <- p :: c.v_parents

let disconnect p c =
  p.v_children <- List.filter (( != ) c) p.v_children;
  p.v_parents <- List.filter (( != ) p) c.v_parents

let get v = v.v_label

let set v x = v.v_label <- x

let parents v = v.v_parents

let children v = v.v_children

let neighbors v = v.v_parents @ v.v_children

let edges v =
  List.map (fun p -> (p, v)) v.v_parents
  @ List.map (fun c -> (v, c)) v.v_children

let size g = fold (fun acc _ -> succ acc) 0 g

let validate g =
  fold (fun acc v ->
      acc && List.for_all (fun c -> List.mem v c.v_parents) v.v_children)
    true g

(** {2 Pretty printing} *)

let pp_graph pp_vertex ppf g =
  let ptr x = Obj.magic x * 2 in
  let print_edge lhs rhs =
    fprintf ppf "  v%d -> v%d [arrowhead=normal];@\n" (ptr lhs) (ptr rhs)
  in
  fprintf ppf "digraph _ {@\n";
  iter (fun v ->
      fprintf ppf "  v%d [%a];@\n" (ptr v) pp_vertex v;
      List.iter (print_edge v) v.v_children) g;
  fprintf ppf "}"

let output_image pp_vertex filename g =
  let oc = Unix.open_process_out ("dot -Tsvg -o " ^ filename) in
  let ppf = formatter_of_out_channel oc in
  pp_graph pp_vertex ppf g;
  pp_print_flush ppf ();
  match Unix.close_process_out oc with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith "process failure"
