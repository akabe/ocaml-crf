open Format
open Slap.D
open Slap.Io
open Crf
open Test_common

let print_table title v1 v2 =
  let get_el i = function
    | 1 -> Vec.get_dyn v1 i
    | 2 -> Vec.get_dyn v2 i
    | 3 -> abs_float (Vec.get_dyn v1 i -. Vec.get_dyn v2 i)
    | _ -> assert(false)
  in
  let head = [|"Naive"; "Sampling"; "Error"|] in
  printf "%s: Error = %g@." title (Vec.ssqr_diff v1 v2);
  pp_table
    ~pp_head:(fun ppf i -> pp_print_string ppf head.(i-1))
    ~pp_left:(fun ppf i -> pp_print_int ppf i)
    ~pp_end_col:(fun ppf ~row:_ ~col:_ -> pp_print_string ppf " | ")
    (fun ppf -> fprintf ppf "%.6g") std_formatter
    (Slap.Size.to_int (Vec.dim v1)) 3 get_el;
  printf "@\n@."

let main () =
  let w = random_fwvec ~rng model in
  (* Normalization coefficient *)
  let n1 = Naive.normalizer model w input_graph in
  let n2 = Sampling.normalizer ~rng Sampling.default model w input_graph in
  printf "normalizer:@\n  \
          Naive    = %.6g@\n  \
          Sampling = %.6g@\n  \
          Error    = %.6g@\n@." n1 n2 (abs_float (n1 -. n2));
  (* Means of feature vectors *)
  let mfv1 = Naive.mean_feature_vec ~all:true model w input_graph in
  let mfv2 = Sampling.mean_feature_vec
      ~rng ~all:true Sampling.default model w input_graph in
  print_table "mean_feature_vec (observable & latent vars)" mfv1 mfv2;
  let mfv1' = Naive.mean_feature_vec ~all:false model w input_graph in
  let mfv2' = Sampling.mean_feature_vec
      ~rng ~all:false Sampling.default model w input_graph in
  print_table "mean_feature_vec (latent vars only)" mfv1' mfv2';
  (* The gradient of log likelihood *)
  let dJ1 = Naive.grad_log_likelihood model w input_graph in
  let dJ2 = Sampling.grad_log_likelihood
      ~rng Sampling.default model w input_graph in
  print_table "grad_log_likelihood" dJ1 dJ2
