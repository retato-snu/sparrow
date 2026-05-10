let modules = ref []
let out_dir = ref ""

let add_module path = modules := !modules @ [path]

let usage = "real_preanalysis_observer --module <file.c>... --out <dir>"

let boundary = "Global.init -> PreAnalysis.perform"
let schema_version = "real-sparrow-preanalysis/v1"

let rec mkdir_p path =
  if path <> "" && path <> "." && not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let write_json path json =
  let dir = Filename.dirname path in
  mkdir_p dir;
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string json);
  output_char oc '\n';
  close_out oc

let sort_strings xs = List.sort_uniq String.compare xs
let node_string = InterCfg.Node.to_string

let sorted_pids global = global.Global.icfg |> InterCfg.pidsof |> sort_strings

let bta_of_cmd cmd =
  match cmd with
  | IntraCfg.Cmd.Cexternal _ -> "dynamic"
  | _ -> "static"

let bta_of_node global node cmd =
  match cmd with
  | IntraCfg.Cmd.Cexternal _ -> "dynamic"
  | IntraCfg.Cmd.Ccall (_, Sparrow_cil.Lval (Sparrow_cil.Var callee, Sparrow_cil.NoOffset), _, _) ->
    let callees = InterCfg.get_callees node global.Global.icfg in
    if not (InterCfg.ProcSet.is_empty callees) || List.mem callee.Sparrow_cil.vname (sorted_pids global)
    then "static"
    else "residual"
  | IntraCfg.Cmd.Ccall _ ->
    let callees = InterCfg.get_callees node global.Global.icfg in
    if InterCfg.ProcSet.is_empty callees then "residual" else "static"
  | _ -> bta_of_cmd cmd

let depends_on_extern_bta bta = bta <> "static"
let depends_on_extern_cmd cmd = depends_on_extern_bta (bta_of_cmd cmd)

let bta_reason = function
  | "dynamic" -> "external input command"
  | "residual" -> "unresolved external call at module-only boundary"
  | _ -> "module-local PreAnalysis command"

let command_kind cmd =
  let open IntraCfg.Cmd in
  match cmd with
  | Cinstr _ -> "Cinstr"
  | Cif _ -> "Cif"
  | CLoop _ -> "CLoop"
  | Cset _ -> "Cset"
  | Cexternal _ -> "Cexternal"
  | Calloc _ -> "Calloc"
  | Csalloc _ -> "Csalloc"
  | Cfalloc _ -> "Cfalloc"
  | Cassume _ -> "Cassume"
  | Ccall _ -> "Ccall"
  | Creturn _ -> "Creturn"
  | Casm _ -> "Casm"
  | Cskip -> "Cskip"

let weak_step_lineage global =
  global.Global.icfg
  |> InterCfg.nodesof
  |> List.map (fun node ->
       let cmd = InterCfg.cmdof global.Global.icfg node in
       `Assoc [
         "procedure", `String (InterCfg.Node.get_pid node);
         "node", `String (node_string node);
         "command_kind", `String (command_kind cmd);
         "command", `String (IntraCfg.Cmd.to_string cmd);
         "transfer", `String "ItvSem.run";
         "mode", `String "Weak";
         "spec", `String "ItvSem.Spec.empty";
         "state_summary_id", `String (node_string node);
         "bta", `String (bta_of_node global node cmd);
       ])
  |> List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))

let direct_call_edges global =
  global.Global.icfg
  |> InterCfg.callnodesof
  |> List.concat_map (fun node ->
       let callees = InterCfg.get_callees node global.Global.icfg in
       InterCfg.ProcSet.fold (fun callee acc ->
         (`List [`String (node_string node); `String (InterCfg.Proc.to_string callee)]) :: acc)
         callees [])
  |> List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))

let transitive_call_edges global =
  let pids = InterCfg.global_proc :: sorted_pids global in
  pids
  |> List.concat_map (fun caller ->
       let callees = CallGraph.trans_callees caller global.Global.callgraph in
       BasicDom.PowProc.fold (fun callee acc ->
         (`List [`String caller; `String (BasicDom.Proc.to_string callee)]) :: acc)
         callees [])
  |> List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))

let unresolved_calls global =
  global.Global.icfg
  |> InterCfg.callnodesof
  |> List.filter_map (fun node ->
       let callees = InterCfg.get_callees node global.Global.icfg in
       if InterCfg.ProcSet.is_empty callees then
         Some (`Assoc [
           "procedure", `String (InterCfg.Node.get_pid node);
           "node", `String (node_string node);
           "reason", `String "no-callee-in-preanalysis-projection";
           "bta", `String "residual";
         ])
       else None)
  |> List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))

let memory_summary mem =
  ItvDom.Mem.fold (fun loc value acc ->
    (`Assoc [
      "location", `String (BasicDom.Loc.to_string loc);
      "value", `String (ItvDom.Val.to_string value);
      "bta", `String (if BasicDom.Loc.is_ext_allocsite loc then "dynamic" else "static");
    ]) :: acc)
    mem []
  |> List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))

let staging_status_of_bta = function
  | "static" -> "completed-at-preanalysis-boundary"
  | "dynamic" -> "extern-dependent"
  | _ -> "residual"

let result_of_mem_change input output =
  if ItvDom.Mem.eq output ItvDom.Mem.bot then "infeasible"
  else if ItvDom.Mem.eq input output then "preserved"
  else "refined"

let prune_sites before after =
  let input_mem = after.Global.mem in
  before.Global.icfg
  |> InterCfg.nodesof
  |> List.filter_map (fun node ->
       match InterCfg.cmdof before.Global.icfg node with
       | IntraCfg.Cmd.Cassume (condition, _) as cmd ->
         let output_mem, _ =
           ItvSem.run AbsSem.Weak ItvSem.Spec.empty node (input_mem, after)
         in
         let bta = bta_of_cmd cmd in
         let input_card = ItvDom.Mem.cardinal input_mem in
         let output_card = ItvDom.Mem.cardinal output_mem in
         let result = result_of_mem_change input_mem output_mem in
         Some (`Assoc [
           "procedure", `String (InterCfg.Node.get_pid node);
           "node", `String (node_string node);
           "command", `String (IntraCfg.Cmd.to_string cmd);
           "condition", `String (CilHelper.s_exp condition);
           "input_memory_cardinality", `Int input_card;
           "output_memory_cardinality", `Int output_card;
           "result", `String result;
           "result_summary", `String (Printf.sprintf "%s:%d->%d" result input_card output_card);
           "bta", `String bta;
           "depends_on_extern", `Bool (depends_on_extern_cmd cmd);
           "staging_status", `String (staging_status_of_bta bta);
         ])
       | _ -> None)
  |> List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))

let bta_facts global =
  global.Global.icfg
  |> InterCfg.nodesof
  |> List.map (fun node ->
       let cmd = InterCfg.cmdof global.Global.icfg node in
       let bta = bta_of_node global node cmd in
       let is_dynamic = depends_on_extern_bta bta in
       `Assoc [
         "id", `String (node_string node);
         "kind", `String (command_kind cmd);
         "classification", `String bta;
         "reason", `String (bta_reason bta);
         "depends_on_extern", `Bool is_dynamic;
       ])
  |> List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))

let artifact_for_global ~source before after =
  let before_pids = sorted_pids before in
  let after_pids = sorted_pids after in
  let pruned = before_pids |> List.filter (fun p -> not (List.mem p after_pids)) in
  `Assoc [
    "schema_version", `String schema_version;
    "source", `String source;
    "scope", `String "module-only";
    "boundary", `String boundary;
    "lineage", `Assoc [
      "preanalysis", `String "sparrow/src/core/preAnalysis.ml:19-74";
      "itv_sem_run", `String "sparrow/src/semantics/itvSem.ml:811-899";
      "mode", `String "AbsSem.Weak";
      "spec", `String "ItvSem.Spec.empty";
    ];
    "projection", `Assoc [
      "weak_step_lineage", `List (weak_step_lineage before);
      "callgraph", `Assoc [
        "direct_edges", `List (direct_call_edges after);
        "transitive_edges", `List (transitive_call_edges after);
        "unresolved_calls", `List (unresolved_calls after);
      ];
      "pruning", `Assoc [
        "before_functions", `List (List.map (fun p -> `String p) before_pids);
        "after_functions", `List (List.map (fun p -> `String p) after_pids);
        "reachable_functions", `List (List.map (fun p -> `String p) after_pids);
        "pruned_functions", `List (List.map (fun p -> `String p) pruned);
        "prune_sites", `List (prune_sites before after);
      ];
      "memory_summary", `List (memory_summary after.Global.mem);
      "bta", `List (bta_facts before);
    ];
    "non_claims", `List [
      `String "no Strong-mode staging";
      `String "no sparse/DUG parity";
      `String "no whole-program merge equivalence";
      `String "no executable residual linker/global-fixpoint evidence";
    ];
  ]

let artifact_for_module source =
  Frontend.files := [source];
  let before = () |> Frontend.parse |> Frontend.makeCFGinfo |> Global.init in
  let after = PreAnalysis.perform before in
  artifact_for_global ~source before after

let artifact_path out_dir source =
  Filename.concat out_dir (Filename.basename source ^ ".preanalysis.json")

let write_module source =
  let path = artifact_path !out_dir source in
  write_json path (artifact_for_module source);
  path

let () =
  Arg.parse
    [
      "--module", Arg.String add_module, "C source module to run through frozen Sparrow Global.init -> PreAnalysis.perform";
      "--out", Arg.Set_string out_dir, "output directory";
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !modules = [] then failwith "at least one --module is required";
  if !out_dir = "" then failwith "--out is required";
  Sparrow_cil.initCIL ();
  let paths = List.map write_module !modules in
  let manifest = `Assoc [
    "schema_version", `String schema_version;
    "modules", `List (List.map (fun path -> `String path) paths);
    "count", `Int (List.length paths);
    "boundary", `String boundary;
  ] in
  write_json (Filename.concat !out_dir "manifest.json") manifest;
  List.iter print_endline paths
