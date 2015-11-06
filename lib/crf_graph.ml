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

(** {2 Iterators} *)

let connect_to_children v children =
  List.iter (fun c -> c.v_parents <- v :: c.v_parents) children;
  v.v_children <- children

let map f g =
  let tbl = ref [] in
  let rec aux v =
    try List.assq v !tbl
    with Not_found ->
      let v' = { v_label = f v; v_parents = []; v_children = []; } in
      tbl := (v, v') :: !tbl;
      connect_to_children v' (List.map aux v.v_children);
      v'
  in
  aux g

let map2 f g1 g2 =
  let tbl = ref [] in
  let rec aux v1 v2 =
    try List.assq v1 !tbl
    with Not_found ->
      let v' = { v_label = f v1 v2; v_parents = []; v_children = []; } in
      tbl := (v1, v') :: !tbl;
      connect_to_children v' (List.map2 aux v1.v_children v2.v_children);
      v'
  in
  aux g1 g2

let fold f acc g =
  let seen = ref [] in
  let rec aux acc v =
    if List.memq v !seen then acc else begin
      seen := v :: !seen;
      List.fold_left aux (f acc v) v.v_children
    end
  in
  aux acc g

let fold2 f acc g1 g2 =
  let seen = ref [] in
  let rec aux acc v1 v2 =
    if List.memq v1 !seen then acc else begin
      seen := v1 :: !seen;
      List.fold_left2 aux (f acc v1 v2) v1.v_children v2.v_children
    end
  in
  aux acc g1 g2

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

(** {2 Comparison} *)

let rec cmp_list cmp xs ys = match xs, ys with
  | [], [] -> 0
  | _ :: _, [] -> 1
  | [], _ :: _ -> -1
  | x :: xs', y :: ys' ->
    let res = cmp x y in
    if res = 0 then cmp_list cmp xs' ys' else res

let compare ?(cmp = Pervasives.compare) =
  let seen = ref [] in
  let rec aux v1 v2 =
    if List.memq v1 !seen then 0 else begin
      seen := v1 :: !seen;
      let res = cmp (get v1) (get v2) in
      if res = 0 then cmp_list aux v1.v_children v2.v_children else res
    end
  in
  aux

let equal ?cmp x y = compare ?cmp x y = 0

(** {2 Pretty printing} *)

let pp_graph pp_vertex ppf g =
  let tbl = ref [] in
  let index v =
    let n = List.length !tbl in
    let rec aux i = function
      | [] -> tbl := v :: !tbl ; n
      | hd :: tl -> if hd == v then i else aux (i - 1) tl
    in
    aux (n - 1) !tbl
  in
  let print_edge lhs rhs =
    fprintf ppf "  v%d -> v%d [arrowhead=normal];@\n" (index lhs) (index rhs)
  in
  fprintf ppf "digraph _ {@\n";
  iter (fun v ->
      fprintf ppf "  v%d [%a];@\n" (index v) pp_vertex v;
      List.iter (print_edge v) v.v_children) g;
  fprintf ppf "}"

let output_image pp_vertex filename g =
  let oc = Unix.open_process_out ("dot -Tsvg -o " ^ filename) in
  let ppf = formatter_of_out_channel oc in
  pp_graph pp_vertex ppf g;
  pp_print_flush ppf ();
  match Unix.close_process_out oc with
  | Unix.WEXITED 0 -> ()
  | _ -> (* Process failure *)
     let oc = open_out (filename ^ ".dot") in
     let ppf = formatter_of_out_channel oc in
     pp_graph pp_vertex ppf g;
     pp_print_flush ppf ();
     close_out oc
