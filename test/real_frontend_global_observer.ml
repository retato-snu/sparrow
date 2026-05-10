let modules = ref []
let out_dir = ref ""

let add_module path = modules := !modules @ [path]

let usage = "real_frontend_global_observer --module <file.c>... --out <dir>"

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

let artifact_path out_dir source =
  Filename.concat out_dir (Filename.basename source ^ ".frontend-global.json")

let artifact_for_module source =
  Frontend.files := [source];
  let global =
    ()
    |> Frontend.parse
    |> Frontend.makeCFGinfo
    |> Global.init
  in
  `Assoc [
    "source", `String source;
    "parser", `String "Sparrow Frontend.parseOneFile via GoblintCIL Frontc";
    "lineage", `Assoc [
      "frontend", `String "sparrow/src/core/frontend.ml";
      "global", `String "sparrow/src/program/global.ml";
      "inter_cfg", `String "sparrow/src/program/interCfg.ml";
      "path", `String "parseOneFile -> makeCFGinfo -> Global.init";
    ];
    "global", `Assoc [
      "lineage", `Assoc [
        "source", `String "sparrow/src/program/global.ml";
        "boundary", `String "Global.init";
        "callgraph", `String "CallGraph.empty at this boundary";
        "analysis_fields", `String "support-only bottoms; not analysis PE evidence";
        "inter_cfg", `String "frozen sparrow/src/program/interCfg.ml with IntraCfg.init";
      ];
      "file", `String source;
      "global_json", `Assoc [
        "callgraph", CallGraph.to_json global.Global.callgraph;
        "cfgs", InterCfg.to_json global.Global.icfg;
      ];
    ];
  ]

let write_module source =
  let path = artifact_path !out_dir source in
  write_json path (artifact_for_module source);
  path

let () =
  Arg.parse
    [
      "--module", Arg.String add_module, "C source module to parse with frozen Sparrow frontend/global";
      "--out", Arg.Set_string out_dir, "output directory";
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !modules = [] then failwith "at least one --module is required";
  if !out_dir = "" then failwith "--out is required";
  Sparrow_cil.initCIL ();
  let paths = List.map write_module !modules in
  let manifest =
    `Assoc [
      "modules", `List (List.map (fun path -> `String path) paths);
      "count", `Int (List.length paths);
      "path", `String "frozen Sparrow parseOneFile -> makeCFGinfo -> Global.init";
    ]
  in
  write_json (Filename.concat !out_dir "manifest.json") manifest;
  List.iter print_endline paths
