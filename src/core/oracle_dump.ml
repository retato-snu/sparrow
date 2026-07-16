(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(*                                                                     *)
(***********************************************************************)

let fail_usage msg =
  prerr_endline ("sparrow-oracle-dump: " ^ msg);
  exit 2

let starts_with_dash s =
  String.length s > 0 && s.[0] = '-'

let reject_non_oracle_options () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  let rec loop i =
    if i >= argc then ()
    else
      match argv.(i) with
      | "--stage" | "--out" | "--worklist_order" | "--preanalysis_order" ->
        if i + 1 >= argc then fail_usage ("missing value for " ^ argv.(i))
        else loop (i + 2)
      | "--harness" | "--module" | "--stable_node_ids" | "--static_rename"
      | "--oracle_align" -> loop (i + 1)
      | arg when starts_with_dash arg ->
        fail_usage ("unsupported v1 oracle option: " ^ arg)
      | _ -> loop (i + 1)
  in
  loop 1

let parse_stage = function
  | "front_end" -> SparrowOracleDump.FrontEnd
  | "pre" -> SparrowOracleDump.Pre
  | "sparse" -> SparrowOracleDump.Sparse
  | stage -> fail_usage ("unsupported stage: " ^ stage)

let main () =
  reject_non_oracle_options ();
  let stage = ref None in
  let out = ref None in
  let harness = ref false in
  let module_view = ref false in
  let usage = "Usage: sparrow-oracle-dump --stage front_end|pre|sparse --out oracle.json [--harness] [--module] [--stable_node_ids] [--static_rename] [--oracle_align] source-files" in
  let specs = [
    ("--stage", Arg.String (fun s -> stage := Some (parse_stage s)), "Oracle stage");
    ("--out", Arg.String (fun s -> out := Some s), "Output oracle JSON path");
    ("--harness", Arg.Set harness, "Synthesize a main calling all defined functions (for main-less library TUs)");
    ("--module", Arg.Set module_view, "Module (translation-unit) front-end view: no-main _G_ that initializes globals and calls nothing (front_end stage only)");
    ("--stable_node_ids", Arg.Set Options.stable_node_ids, "Content-stable per-function node/statement ids (A1 front-end normalization)");
    ("--static_rename", Arg.Set Options.static_rename, "Qualify file-static symbols as name@<module_key(path)> per TU (A1 front-end normalization)");
    ("--oracle_align", Arg.Unit (fun () -> Options.stable_node_ids := true; Options.static_rename := true), "Oracle-aligned mode: both A1 normalizations, for node-key comparability");
    ("--worklist_order", Arg.Set_string Options.worklist_order, "Worklist order strategy: wto (default) | file");
    ("--preanalysis_order", Arg.Set_string Options.preanalysis_order, "Pre-analysis node traversal: default | file");
  ] in
  Arg.parse specs Frontend.args usage;
  (* Canonical source-file order: the modular front end sorts files before
     Mergecil.merge (so the merged _G_ is independent of CLI order); the
     oracle must merge in the SAME canonical order or multi-file front_end
     equality breaks whenever CLI order differs from sorted order. *)
  Frontend.files := List.sort String.compare !Frontend.files;
  let stage =
    match !stage with
    | Some stage -> stage
    | None -> fail_usage "missing --stage"
  in
  let out =
    match !out with
    | Some out -> out
    | None -> fail_usage "missing --out"
  in
  (if !module_view then
     match stage with
     | SparrowOracleDump.FrontEnd -> Options.frontend_module := true
     | _ -> fail_usage "--module is a front_end-stage view only");
  Sparrow_cil.initCIL ();
  let global =
    match stage with
    | SparrowOracleDump.FrontEnd -> SparrowPipeline.front_end_input ()
    | SparrowOracleDump.Pre
    | SparrowOracleDump.Sparse ->
      StepManager.stepf true "Front-end" Frontend.parse ()
      |> (if !harness then Frontend.build_main_harness else fun f -> f)
      |> Frontend.makeCFGinfo
      |> SparrowPipeline.pre_result
  in
  match stage with
  | SparrowOracleDump.Sparse ->
    SparrowOracleDump.write_sparse out !Frontend.files global
  | _ ->
    SparrowOracleDump.write out stage !Frontend.files global

let _ =
  try main () with exc ->
    prerr_endline (Printexc.to_string exc);
    prerr_endline (Printexc.get_backtrace ());
    exit 1
