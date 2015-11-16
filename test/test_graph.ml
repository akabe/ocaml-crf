open Format
open Crf

let pp_label ppf = function
  | { in_label; out_label = None; } -> fprintf ppf "%b,none" in_label
  | { in_label; out_label = Some b; } -> fprintf ppf "%b,%b" in_label b

let parse = function
  | "true,none" -> { in_label = true; out_label = None; }
  | "true,true" -> { in_label = true; out_label = Some true; }
  | "true,false" -> { in_label = true; out_label = Some false; }
  | "false,none" -> { in_label = false; out_label = None; }
  | "false,true" -> { in_label = false; out_label = Some true; }
  | "false,false" -> { in_label = false; out_label = Some false; }
  | _ -> assert false

let main () =
  let s = asprintf "%a" (Crf.Graph.save pp_label)
      Test_common.input_graph in
  printf "%s@." s;
  let g = Crf.Graph.load_from_string parse s in
  printf "%a@." (Crf.Graph.save pp_label) g
