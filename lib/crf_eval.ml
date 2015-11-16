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
open Crf_model

type 'a key =
  | Label of 'a
  | Total

type 'a confusion_matrix =
  {
    labels : 'a array;
    tbl : ('a key * 'a key, int) Hashtbl.t;
  }

let confusion_matrix model igs ogs =
  let labels = model.out_labels in
  let tbl = Hashtbl.create 16 in
  Hashtbl.add tbl (Total, Total) 0;
  Array.iter (fun x ->
    Hashtbl.add tbl (Label x, Total) 0;
    Hashtbl.add tbl (Total, Label x) 0;
    Array.iter (fun y -> Hashtbl.add tbl (Label x, Label y) 0) labels)
    labels;
  let incr key =
    let n = Hashtbl.find tbl key in
    Hashtbl.replace tbl key (succ n)
  in
  let aux iv ov =
    match Crf_graph.get iv with
    | { out_label = None; _ } -> ()
    | { out_label = Some ans; _ } ->
      let pred = Crf_graph.get ov in (* prediction *)
      incr (Label ans, Label pred);
      incr (Label ans, Total);
      incr (Total, Label pred);
      incr (Total, Total)
  in
  List.iter2 (Crf_graph.iter2 aux) igs ogs;
  { labels; tbl; }

let pp_confusion_matrix pp_out_label ppf { labels; tbl } =
  let n = Array.length labels in
  let pp_label ppf i =
    if i <= n then pp_out_label ppf labels.(i-1)
    else pp_print_string ppf "Total"
  in
  let pp_end_col ppf ~row:_ ~col:_ = pp_print_string ppf " | " in
  let get_el i j =
    let k1 = if i <= n then Label labels.(i-1) else Total in
    let k2 = if j <= n then Label labels.(j-1) else Total in
    Hashtbl.find tbl (k1, k2)
  in
  Slap.Io.pp_table ~pp_end_col ~pp_left:pp_label ~pp_head:pp_label
    pp_print_int ppf (n + 1) (n + 1) get_el

let accuracy { labels; tbl; } =
  let deno = Hashtbl.find tbl (Total, Total) in
  let nume = Array.fold_left
      (fun acc x -> acc + Hashtbl.find tbl (Label x, Label x))
      0 labels in
  float nume /. float deno

let precision { labels; tbl; } =
  Array.map (fun x ->
      let deno = Hashtbl.find tbl (Total, Label x) in
      let nume = Hashtbl.find tbl (Label x, Label x) in
      float nume /. float deno)
    labels

let recall { labels; tbl; } =
  Array.map (fun x ->
      let deno = Hashtbl.find tbl (Label x, Total) in
      let nume = Hashtbl.find tbl (Label x, Label x) in
      float nume /. float deno)
    labels

let f_score { labels; tbl; } =
  Array.map (fun x ->
      let denoP = Hashtbl.find tbl (Total, Label x) in
      let denoR = Hashtbl.find tbl (Label x, Total) in
      let nume = Hashtbl.find tbl (Label x, Label x) in
      let prec = float nume /. float denoP in
      let recall = float nume /. float denoR in
      2.0 *. prec *. recall /. (prec +. recall))
    labels

let pp_score pp_out_label ppf cmat =
  let precs = precision cmat in
  let recalls = recall cmat in
  let fs = f_score cmat in
  let n = Array.length cmat.labels in
  let pp_left ppf i = pp_out_label ppf cmat.labels.(i-1) in
  let pp_head ppf i =
    pp_print_string ppf [|"Precision"; "Recall"; "F-score"|].(i-1)
  in
  let pp_end_col ppf ~row:_ ~col:_ = pp_print_string ppf " | " in
  let get_el i j = [|precs; recalls; fs|].(j-1).(i-1) in
  Slap.Io.pp_table ~pp_end_col ~pp_left ~pp_head
    (fun ppf -> fprintf ppf "%g") ppf n 3 get_el
