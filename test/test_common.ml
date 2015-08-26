open Format
open Slap.D
open Slap.Io
open Crf

module G = Graph

module KV = (val Slap.Size.of_int_dyn 4 : Slap.Size.SIZE)
module KE = (val Slap.Size.of_int_dyn 16 : Slap.Size.SIZE)

let rng = Gsl.Rng.make Gsl.Rng.MT19937

(* Model definition *)
let model =
  let idx_of_label b = if b then 1 else 0 in
  let idx_of_vertex iv ov =
    idx_of_label (G.get iv).in_label * 2 + idx_of_label (G.get ov) in
  let vertex_ff iv ov =
    let j = idx_of_vertex iv ov in
    Vec.init KV.value (fun i -> if i = j then 1.0 else 0.0)
  in
  let edge_ff iv1 ov1 iv2 ov2 =
    let j = (idx_of_vertex iv1 ov1) * 4 + (idx_of_vertex iv2 ov2) in
    Vec.init KE.value (fun i -> if i = j then 1.0 else 0.0)
  in
  { out_labels = [|false; true|];
    vertex_dim = KV.value; edge_dim = KE.value;
    vertex_ff; edge_ff; }

(* Graph definition

   +--- v1 --- v2 --- v3 --- v5 ---+
   |           |             |     |
   |           +----- v4 ----+     |
   +-------------------------------+ *)
let input_graph =
  let vertex il ol = G.create { in_label = il; out_label = ol; } in
  let v1 = vertex true None in
  let v2 = vertex false (Some true) in
  let v3 = vertex true (Some false) in
  let v4 = vertex false None in
  let v5 = vertex false None in
  G.connect v1 v2;
  G.connect v2 v3;
  G.connect v2 v4;
  G.connect v3 v5;
  G.connect v4 v5;
  G.connect v5 v1;
  v1
