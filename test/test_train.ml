open Format
open Slap.D
open Slap.Io
open Crf
open Test_common

let string_of_og og =
  let string_of_bool = function
    | true -> "T"
    | false -> "F"
  in
  G.fold (fun acc ov -> acc ^ string_of_bool (G.get ov)) "" og

let show_all_patterns model w ig =
  Crf_naive.fold ~all:false model
    (fun () og ->
       let pot = graph_potential model w ig og in
       printf "Pattern = %s, Potential = %g@." (string_of_og og) pot)
    () ig

let pp_out_vertex ppf ov =
  match G.get ov with
  | false -> pp_print_string ppf "label=\"false\""
  | true -> pp_print_string ppf "label=\"true\""

let main () =
  let data_set = [input_graph] in
  let sigma2 = 1.0 in
  let eta = 0.02 in
  let w = random_fwvec ~rng model in
  printf "Training:@.";
  for i = 0 to 1000 do
    let dJ_dw = grad_log_posterior
        ~rng ~sigma2 default_sampler model w data_set in
    axpy ~alpha:eta dJ_dw w;
    if i mod 100 = 0 then printf "Loop %4d: |dJ/dw|^2 = %g@." i (nrm2 dJ_dw)
  done;
  show_all_patterns model w input_graph;
  let og' = Crf_sa.infer ~rng ~all:false default_sa model w input_graph in
  printf "graph = %s; potential = %g@."
    (string_of_og og') (graph_potential model w input_graph og')
