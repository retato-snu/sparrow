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
open Yojson.Safe
open Sparrow_cil
open BasicDom

type stage = FrontEnd | Pre | Sparse

let string_of_stage = function
  | FrontEnd -> "front_end"
  | Pre -> "pre"
  | Sparse -> "sparse"

let assoc xs = `Assoc xs
let list xs = `List xs
let str x = `String x
let int x = `Int x
let bool x = `Bool x

let opt f = function
  | None -> `Null
  | Some x -> f x

let compare_by f x y = compare (f x) (f y)
let sort_by f xs = List.sort (compare_by f) xs

let uniq_sorted compare xs =
  let rec loop acc = function
    | x :: y :: rest when compare x y = 0 -> loop acc (y :: rest)
    | x :: rest -> loop (x :: acc) rest
    | [] -> List.rev acc
  in
  loop [] xs

let sorted_strings xs =
  xs |> List.sort compare |> uniq_sorted compare

let json_string_list xs =
  xs |> sorted_strings |> List.map str |> list

let starts_with prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length
  && String.sub value 0 prefix_length = prefix

let type_id typ =
  "type:" ^
  Digest.to_hex (Digest.string (Marshal.to_string (Sparrow_cil.typeSig typ) []))

let json_type typ =
  assoc [
    ("id", str (type_id typ));
    ("text", str (CilHelper.s_type typ));
  ]

let storage_to_string = function
  | NoStorage -> "none"
  | Static -> "static"
  | Register -> "register"
  | Extern -> "extern"

let ikind_to_string = function
  | IChar -> "char"
  | ISChar -> "signed_char"
  | IUChar -> "unsigned_char"
  | IBool -> "bool"
  | IInt -> "int"
  | IUInt -> "unsigned_int"
  | IShort -> "short"
  | IUShort -> "unsigned_short"
  | ILong -> "long"
  | IULong -> "unsigned_long"
  | ILongLong -> "long_long"
  | IULongLong -> "unsigned_long_long"
  | IInt128 -> "int128"
  | IUInt128 -> "unsigned_int128"

let fkind_to_string = function
  | FFloat -> "float"
  | FDouble -> "double"
  | FLongDouble -> "long_double"
  | FFloat128 -> "float128"
  | FFloat16 -> "float16"
  | FComplexFloat -> "complex_float"
  | FComplexDouble -> "complex_double"
  | FComplexLongDouble -> "complex_long_double"
  | FComplexFloat128 -> "complex_float128"
  | FComplexFloat16 -> "complex_float16"

let castkind_to_string = function
  | Explicit -> "explicit"
  | IntegerPromotion -> "integer_promotion"
  | DefaultArgumentPromotion -> "default_argument_promotion"
  | ArithmeticConversion -> "arithmetic_conversion"
  | ConditionalConversion -> "conditional_conversion"
  | PointerConversion -> "pointer_conversion"
  | Implicit -> "implicit"
  | Internal -> "internal"

let location loc =
  assoc [
    ("file", str loc.file);
    ("line", int loc.line);
    ("byte", int loc.byte);
    ("column", int loc.column);
    ("end_line", int loc.endLine);
    ("end_byte", int loc.endByte);
    ("end_column", int loc.endColumn);
    ("synthetic", bool loc.synthetic);
    ("display", str (CilHelper.s_location loc));
  ]

let varinfo vi =
  assoc [
    ("name", str vi.vname);
    ("vid", int vi.vid);
    ("type", json_type vi.vtype);
    ("storage", str (storage_to_string vi.vstorage));
    ("global", bool vi.vglob);
    ("inline", bool vi.vinline);
    ("decl", location vi.vdecl);
    ("address_taken", bool vi.vaddrof);
    ("referenced", bool vi.vreferenced);
    ("has_decl_instruction", bool vi.vhasdeclinstruction);
  ]

let rec exp e =
  let diagnostic = ("pretty", str (CilHelper.s_exp e)) in
  match e with
  | Const c -> assoc [("kind", str "const"); ("constant", constant c); diagnostic]
  | Lval lv -> assoc [("kind", str "lval"); ("lval", lval lv); diagnostic]
  | SizeOf typ -> assoc [("kind", str "sizeof_type"); ("type", json_type typ); diagnostic]
  | SizeOfE e -> assoc [("kind", str "sizeof_exp"); ("exp", exp e); diagnostic]
  | SizeOfStr s -> assoc [("kind", str "sizeof_string"); ("value", str s); diagnostic]
  | AlignOf typ -> assoc [("kind", str "alignof_type"); ("type", json_type typ); diagnostic]
  | AlignOfE e -> assoc [("kind", str "alignof_exp"); ("exp", exp e); diagnostic]
  | Real e -> assoc [("kind", str "real"); ("exp", exp e); diagnostic]
  | Imag e -> assoc [("kind", str "imag"); ("exp", exp e); diagnostic]
  | UnOp (op, e, typ) ->
    assoc [
      ("kind", str "unop");
      ("op", str (CilHelper.s_uop op));
      ("exp", exp e);
      ("type", json_type typ);
      diagnostic;
    ]
  | BinOp (op, e1, e2, typ) ->
    assoc [
      ("kind", str "binop");
      ("op", str (CilHelper.s_bop op));
      ("left", exp e1);
      ("right", exp e2);
      ("type", json_type typ);
      diagnostic;
    ]
  | Question (e1, e2, e3, typ) ->
    assoc [
      ("kind", str "question");
      ("condition", exp e1);
      ("then", exp e2);
      ("else", exp e3);
      ("type", json_type typ);
      diagnostic;
    ]
  | CastE (castkind, typ, e) ->
    assoc [
      ("kind", str "cast");
      ("cast", str (castkind_to_string castkind));
      ("type", json_type typ);
      ("exp", exp e);
      diagnostic;
    ]
  | AddrOf lv -> assoc [("kind", str "addr_of"); ("lval", lval lv); diagnostic]
  | AddrOfLabel stmt_ref ->
    assoc [
      ("kind", str "addr_of_label");
      ("stmt_id", int (!stmt_ref).sid);
      diagnostic;
    ]
  | StartOf lv -> assoc [("kind", str "start_of"); ("lval", lval lv); diagnostic]

and constant c =
  let diagnostic = ("pretty", str (CilHelper.s_const c)) in
  match c with
  | CInt (_, ikind, text) ->
    assoc [
      ("kind", str "int");
      ("ikind", str (ikind_to_string ikind));
      ("text", opt str text);
      diagnostic;
    ]
  | CStr (s, _) -> assoc [("kind", str "string"); ("value", str s); diagnostic]
  | CWStr (ws, _) ->
    assoc [
      ("kind", str "wide_string");
      ("value", list (List.map (fun i -> str (Int64.to_string i)) ws));
      diagnostic;
    ]
  | CChr c -> assoc [("kind", str "char"); ("value", str (String.make 1 c)); diagnostic]
  | CReal (f, fkind, text) ->
    assoc [
      ("kind", str "real");
      ("fkind", str (fkind_to_string fkind));
      ("value", str (string_of_float f));
      ("text", opt str text);
      diagnostic;
    ]
  | CEnum (e, name, enuminfo) ->
    assoc [
      ("kind", str "enum");
      ("name", str name);
      ("enum", str enuminfo.ename);
      ("value", exp e);
      diagnostic;
    ]

and lval (host, off) =
  assoc [
    ("host", lhost host);
    ("offset", offset off);
    ("pretty", str (CilHelper.s_lv (host, off)));
  ]

and lhost = function
  | Var vi -> assoc [("kind", str "var"); ("var", varinfo vi)]
  | Mem e -> assoc [("kind", str "mem"); ("exp", exp e)]

and offset off =
  match off with
  | NoOffset -> assoc [("kind", str "none")]
  | Field (fieldinfo, rest) ->
    assoc [
      ("kind", str "field");
      ("name", str fieldinfo.fname);
      ("comp", str fieldinfo.fcomp.cname);
      ("type", json_type fieldinfo.ftype);
      ("bitfield", opt int fieldinfo.fbitfield);
      ("location", location fieldinfo.floc);
      ("rest", offset rest);
      ("pretty", str (CilHelper.s_offset off));
    ]
  | Index (e, rest) ->
    assoc [
      ("kind", str "index");
      ("exp", exp e);
      ("rest", offset rest);
      ("pretty", str (CilHelper.s_offset off));
    ]

let rec init = function
  | SingleInit e -> assoc [("kind", str "single"); ("exp", exp e)]
  | CompoundInit (typ, entries) ->
    assoc [
      ("kind", str "compound");
      ("type", json_type typ);
      ("entries", list (List.map (fun (off, entry_init) ->
         assoc [("offset", offset off); ("init", init entry_init)]) entries));
    ]

let initinfo info =
  opt init info.init

let comp_field field =
  assoc [
    ("name", str field.fname);
    ("type", json_type field.ftype);
    ("bitfield", opt int field.fbitfield);
    ("location", location field.floc);
  ]

let compinfo comp =
  assoc [
    ("name", str comp.cname);
    ("struct", bool comp.cstruct);
    ("key", int comp.ckey);
    ("defined", bool comp.cdefined);
    ("referenced", bool comp.creferenced);
    ("fields", list (List.map comp_field comp.cfields));
  ]

let enuminfo enum =
  assoc [
    ("name", str enum.ename);
    ("kind", str (ikind_to_string enum.ekind));
    ("referenced", bool enum.ereferenced);
    ("items", list (List.map (fun (name, _, e, loc) ->
       assoc [
         ("name", str name);
         ("value", exp e);
         ("location", location loc);
       ]) enum.eitems));
  ]

let global_item order g =
  let common kind loc fields =
    assoc ([
      ("order", int order);
      ("kind", str kind);
      ("location", location loc);
    ] @ fields)
  in
  match g with
  | GType (ti, loc) ->
    common "typedef" loc [
      ("name", str ti.tname);
      ("type", json_type ti.ttype);
      ("referenced", bool ti.treferenced);
    ]
  | GCompTag (ci, loc) ->
    common "comp_tag" loc [("comp", compinfo ci)]
  | GCompTagDecl (ci, loc) ->
    common "comp_tag_decl" loc [("comp", compinfo ci)]
  | GEnumTag (ei, loc) ->
    common "enum_tag" loc [("enum", enuminfo ei)]
  | GEnumTagDecl (ei, loc) ->
    common "enum_tag_decl" loc [("enum", enuminfo ei)]
  | GVarDecl (vi, loc) ->
    common "var_decl" loc [("var", varinfo vi)]
  | GVar (vi, init_info, loc) ->
    common "var_def" loc [
      ("var", varinfo vi);
      ("init", initinfo init_info);
    ]
  | GFun (fd, loc) ->
    common "function_def" loc [
      ("var", varinfo fd.svar);
      ("formals", list (List.map varinfo fd.sformals));
      ("locals", list (List.map varinfo fd.slocals));
      ("max_id", int fd.smaxid);
      ("max_stmt_id", opt int fd.smaxstmtid);
    ]
  | GAsm (text, loc) ->
    common "asm" loc [("text", str text)]
  | GPragma (_, loc) ->
    common "pragma" loc [("pretty", str "pragma")]
  | GText text ->
    assoc [
      ("order", int order);
      ("kind", str "text");
      ("text", str text);
    ]

let file f =
  let rec globals order = function
    | g :: rest -> global_item order g :: globals (order + 1) rest
    | [] -> []
  in
  assoc [
    ("name", str f.fileName);
    ("globals", list (globals 0 f.globals));
  ]

let proc_compare p1 p2 =
  match p1 = InterCfg.global_proc, p2 = InterCfg.global_proc with
  | true, true -> 0
  | true, false -> -1
  | false, true -> 1
  | false, false -> compare p1 p2

let sorted_procs ps =
  ps |> List.sort proc_compare |> uniq_sorted proc_compare

let node_id = InterCfg.Node.to_string

let sorted_nodes ns =
  ns |> List.sort InterCfg.Node.compare |> uniq_sorted InterCfg.Node.compare

let json_node n =
  assoc [
    ("id", str (node_id n));
    ("procedure", str (InterCfg.Node.get_pid n));
    ("local_id", str (IntraCfg.Node.to_string (InterCfg.Node.get_cfgnode n)));
  ]

let cfg_cmd command =
  let diagnostic = ("pretty", str (IntraCfg.Cmd.to_string command)) in
  match command with
  | IntraCfg.Cmd.Cinstr instrs ->
    assoc [
      ("kind", str "instr");
      ("instructions", list (List.map (fun i -> str (CilHelper.s_instr i)) instrs));
      diagnostic;
    ]
  | IntraCfg.Cmd.Cif (e, _, _, loc) ->
    assoc [("kind", str "if"); ("condition", exp e); ("location", location loc); diagnostic]
  | IntraCfg.Cmd.CLoop loc ->
    assoc [("kind", str "loop"); ("location", location loc); diagnostic]
  | IntraCfg.Cmd.Cset (lv, e, loc) ->
    assoc [
      ("kind", str "set");
      ("lval", lval lv);
      ("exp", exp e);
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Cexternal (lv, loc) ->
    assoc [
      ("kind", str "external");
      ("lval", lval lv);
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Calloc (lv, alloc, is_static, loc) ->
    let alloc =
      match alloc with
      | IntraCfg.Cmd.Array e -> assoc [("kind", str "array"); ("size", exp e)]
      | IntraCfg.Cmd.Struct comp -> assoc [("kind", str "struct"); ("comp", compinfo comp)]
    in
    assoc [
      ("kind", str "alloc");
      ("lval", lval lv);
      ("alloc", alloc);
      ("static", bool is_static);
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Csalloc (lv, s, loc) ->
    assoc [
      ("kind", str "string_alloc");
      ("lval", lval lv);
      ("string", str s);
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Cfalloc (lv, fd, loc) ->
    assoc [
      ("kind", str "function_alloc");
      ("lval", lval lv);
      ("function", str fd.svar.vname);
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Cassume (e, loc) ->
    assoc [
      ("kind", str "assume");
      ("exp", exp e);
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Ccall (lvo, f, args, loc) ->
    assoc [
      ("kind", str "call");
      ("lhs", opt lval lvo);
      ("function", exp f);
      ("args", list (List.map exp args));
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Creturn (eo, loc) ->
    assoc [
      ("kind", str "return");
      ("exp", opt exp eo);
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Casm (_, templates, _, _, clobbers, loc) ->
    assoc [
      ("kind", str "asm");
      ("templates", list (List.map str templates));
      ("clobbers", list (List.map str clobbers));
      ("location", location loc);
      diagnostic;
    ]
  | IntraCfg.Cmd.Cskip ->
    assoc [("kind", str "skip"); diagnostic]

let cfg_of_proc icfg pid =
  let cfg = InterCfg.cfgof icfg pid in
  let local_nodes =
    IntraCfg.nodesof cfg |> List.sort IntraCfg.Node.compare
  in
  let node_json n =
    let full_node = InterCfg.Node.make pid n in
    assoc [
      ("id", str (node_id full_node));
      ("local_id", str (IntraCfg.Node.to_string n));
      ("command", cfg_cmd (IntraCfg.find_cmd n cfg));
    ]
  in
  let edges =
    IntraCfg.fold_edges (fun src dst acc ->
      assoc [
        ("src", str (node_id (InterCfg.Node.make pid src)));
        ("dst", str (node_id (InterCfg.Node.make pid dst)));
      ] :: acc) cfg []
    |> sort_by (function
      | `Assoc fields ->
        (match List.assoc "src" fields, List.assoc "dst" fields with
         | `String src, `String dst -> src ^ "\000" ^ dst
         | _ -> "")
      | _ -> "")
  in
  assoc [
    ("procedure", str pid);
    ("entry", str (node_id (InterCfg.entryof icfg pid)));
    ("exit", str (node_id (InterCfg.exitof icfg pid)));
    ("nodes", list (List.map node_json local_nodes));
    ("edges", list edges);
  ]

let icfg icfg =
  let pids = InterCfg.pidsof icfg |> sorted_procs in
  let procedures =
    List.map (fun pid ->
      assoc [
        ("id", str pid);
        ("is_global", bool (pid = InterCfg.global_proc));
        ("formals", list (List.map varinfo (InterCfg.argsof icfg pid)));
      ]) pids
  in
  let call_edges =
    InterCfg.callnodesof icfg
    |> sorted_nodes
    |> List.fold_left (fun acc node ->
      InterCfg.get_callees node icfg
      |> InterCfg.ProcSet.elements
      |> sorted_procs
      |> List.map (fun callee ->
        assoc [("call_node", str (node_id node)); ("callee", str callee)])
      |> fun edges -> edges @ acc) []
    |> sort_by (function
      | `Assoc fields ->
        (match List.assoc "call_node" fields, List.assoc "callee" fields with
         | `String node, `String callee -> node ^ "\000" ^ callee
         | _ -> "")
      | _ -> "")
  in
  assoc [
    ("procedures", list procedures);
    ("cfgs", list (List.map (cfg_of_proc icfg) pids));
    ("call_edges", list call_edges);
  ]

let powproc_elements ps =
  PowProc.elements ps |> sorted_procs

let edge_key = function
  | `Assoc fields ->
    (match List.assoc "src" fields, List.assoc "dst" fields with
     | `String src, `String dst -> src ^ "\000" ^ dst
     | _ -> "")
  | _ -> ""

let callgraph global =
  let pids = InterCfg.pidsof global.Global.icfg |> sorted_procs in
  let edges_of find =
    List.fold_left (fun acc src ->
      find src global.Global.callgraph
      |> powproc_elements
      |> List.map (fun dst -> assoc [("src", str src); ("dst", str dst)])
      |> fun edges -> edges @ acc) [] pids
    |> sort_by edge_key
  in
  let direct_edges = edges_of CallGraph.callees in
  let transitive_edges = edges_of CallGraph.trans_callees in
  let nodes =
    List.fold_left (fun acc edge ->
      match edge with
      | `Assoc fields ->
        (match List.assoc "src" fields, List.assoc "dst" fields with
         | `String src, `String dst -> src :: dst :: acc
         | _ -> acc)
      | _ -> acc) [] (direct_edges @ transitive_edges)
    |> sorted_strings
  in
  assoc [
    ("nodes", list (List.map str nodes));
    ("direct_edges", list direct_edges);
    ("transitive_edges", list transitive_edges);
  ]

let rec type_ids_in_json acc = function
  | `Assoc fields ->
    let acc =
      match List.assoc_opt "id" fields with
      | Some (`String value) when starts_with "type:" value -> value :: acc
      | _ -> acc
    in
    List.fold_left (fun acc (_, value) -> type_ids_in_json acc value) acc fields
  | `List values -> List.fold_left type_ids_in_json acc values
  | _ -> acc

let rec loc_id = function
  | Loc.GVar (name, _) -> "gvar:" ^ name
  | Loc.LVar (pid, name, _) -> "lvar:" ^ pid ^ ":" ^ name
  | Loc.Allocsite allocsite -> "alloc:" ^ Allocsite.to_string allocsite
  | Loc.Field (base, field, typ) ->
    "field:" ^ loc_id base ^ ":" ^ field ^ ":" ^ type_id typ

let loc_json loc =
  let kind =
    match loc with
    | Loc.GVar _ -> "gvar"
    | Loc.LVar _ -> "lvar"
    | Loc.Allocsite _ -> "allocsite"
    | Loc.Field _ -> "field"
  in
  let base_fields =
    [
      ("id", str (loc_id loc));
      ("kind", str kind);
      ("pretty", str (Loc.to_string loc));
    ]
  in
  let typed =
    match Loc.typ loc with
    | None -> base_fields
    | Some typ -> base_fields @ [("type", json_type typ)]
  in
  assoc typed

let powloc locs =
  locs
  |> PowLoc.elements
  |> List.sort Loc.compare
  |> List.map (fun loc -> str (loc_id loc))
  |> list

let powstruct structs =
  StructBlk.PowStruct.elements structs
  |> sorted_strings
  |> List.map str
  |> list

let integer = function
  | Itv.Integer.Int i -> int i
  | Itv.Integer.MInf -> str "-oo"
  | Itv.Integer.PInf -> str "+oo"

let interval itv =
  if Itv.is_bot itv then assoc [("kind", str "bot")]
  else
    assoc [
      ("kind", str "interval");
      ("lower", integer (Itv.lower_integer itv));
      ("upper", integer (Itv.upper_integer itv));
      ("pretty", str (Itv.to_string itv));
    ]

let allocsite_id a = Allocsite.to_string a

let array_block array =
  if ArrayBlk.eq array ArrayBlk.top then assoc [("kind", str "top")]
  else
    let entries =
      ArrayBlk.foldi (fun allocsite info acc ->
        assoc [
          ("allocsite", str (allocsite_id allocsite));
          ("offset", interval info.ArrayBlk.ArrInfo.offset);
          ("size", interval info.ArrayBlk.ArrInfo.size);
          ("stride", interval info.ArrayBlk.ArrInfo.stride);
          ("null_pos", interval info.ArrayBlk.ArrInfo.null_pos);
          ("structure", powstruct info.ArrayBlk.ArrInfo.structure);
        ] :: acc) array []
      |> sort_by (function
        | `Assoc fields ->
          (match List.assoc "allocsite" fields with `String s -> s | _ -> "")
        | _ -> "")
    in
    assoc [("kind", str "map"); ("entries", list entries)]

let struct_block struct_block =
  if StructBlk.eq struct_block StructBlk.top then assoc [("kind", str "top")]
  else
    let entries =
      StructBlk.foldi (fun loc structs acc ->
        assoc [
          ("location", str (loc_id loc));
          ("structures", powstruct structs);
        ] :: acc) struct_block []
      |> sort_by (function
        | `Assoc fields ->
          (match List.assoc "location" fields with `String s -> s | _ -> "")
        | _ -> "")
    in
    assoc [("kind", str "map"); ("entries", list entries)]

let json_value v =
  assoc [
    ("interval", interval (ItvDom.Val.itv_of_val v));
    ("points_to", powloc (ItvDom.Val.pow_loc_of_val v));
    ("array", array_block (ItvDom.Val.array_of_val v));
    ("struct", struct_block (ItvDom.Val.struct_of_val v));
    ("procedures", json_string_list (PowProc.elements (ItvDom.Val.pow_proc_of_val v)));
    ("pretty", str (ItvDom.Val.to_string v));
  ]

let memory mem =
  ItvDom.Mem.foldi (fun loc mem_value acc ->
    assoc [
      ("location", str (loc_id loc));
      ("value", json_value mem_value);
    ] :: acc) mem []
  |> sort_by (function
    | `Assoc fields ->
      (match List.assoc "location" fields with `String s -> s | _ -> "")
    | _ -> "")
  |> list

let table table =
  ItvDom.Table.foldi (fun node mem acc ->
    assoc [
      ("node", str (node_id node));
      ("mem", memory mem);
    ] :: acc) table []
  |> sort_by (function
    | `Assoc fields ->
      (match List.assoc "node" fields with `String s -> s | _ -> "")
    | _ -> "")
  |> list

let dump dump =
  Dump.foldi (fun pid locs acc ->
    assoc [
      ("procedure", str pid);
      ("return_locations", powloc locs);
    ] :: acc) dump []
  |> sort_by (function
    | `Assoc fields ->
      (match List.assoc "procedure" fields with `String s -> s | _ -> "")
    | _ -> "")
  |> list

let locs_of_value v =
  let base = ItvDom.Val.all_loc_of_val v in
  PowLoc.union base (StructBlk.pow_loc_of_struct (ItvDom.Val.struct_of_val v))

let collect_mem_locs mem acc =
  ItvDom.Mem.foldi (fun loc value acc ->
    loc :: (PowLoc.elements (locs_of_value value) @ acc)) mem acc

let collect_table_locs table acc =
  ItvDom.Table.foldi (fun _ mem acc -> collect_mem_locs mem acc) table acc

let collect_dump_locs dump acc =
  Dump.foldi (fun _ locs acc -> PowLoc.elements locs @ acc) dump acc

let locs_of_global global =
  []
  |> collect_mem_locs global.Global.mem
  |> collect_table_locs global.Global.table
  |> collect_dump_locs global.Global.dump
  |> List.sort Loc.compare
  |> uniq_sorted Loc.compare

let procs_of_value value =
  ItvDom.Val.pow_proc_of_val value |> PowProc.elements

let collect_mem_procs mem acc =
  ItvDom.Mem.foldi (fun _ value acc -> procs_of_value value @ acc) mem acc

let collect_table_procs table acc =
  ItvDom.Table.foldi (fun _ mem acc -> collect_mem_procs mem acc) table acc

let collect_dump_procs dump acc =
  Dump.foldi (fun pid _ acc -> pid :: acc) dump acc

let collect_callgraph_procs global acc =
  let pids = InterCfg.pidsof global.Global.icfg in
  List.fold_left
    (fun acc pid ->
       pid
       :: (CallGraph.callees pid global.Global.callgraph |> PowProc.elements)
       @ (CallGraph.trans_callees pid global.Global.callgraph
          |> PowProc.elements)
       @ acc)
    acc pids

let procs_of_global global =
  InterCfg.pidsof global.Global.icfg
  |> collect_mem_procs global.Global.mem
  |> collect_table_procs global.Global.table
  |> collect_dump_procs global.Global.dump
  |> collect_callgraph_procs global
  |> sorted_procs

let rec loc_types loc acc =
  let acc =
    match Loc.typ loc with
    | None -> acc
    | Some typ -> typ :: acc
  in
  match loc with
  | Loc.Field (base, _, typ) -> loc_types base (typ :: acc)
  | _ -> acc

let global_types g acc =
  match g with
  | GType (ti, _) -> ti.ttype :: acc
  | GCompTag (ci, _) | GCompTagDecl (ci, _) ->
    List.fold_left (fun acc field -> field.ftype :: acc) acc ci.cfields
  | GEnumTag _ | GEnumTagDecl _ -> acc
  | GVarDecl (vi, _) | GVar (vi, _, _) -> vi.vtype :: acc
  | GFun (fd, _) ->
    fd.svar.vtype ::
    List.fold_left (fun acc vi -> vi.vtype :: acc) acc (fd.sformals @ fd.slocals)
  | GAsm _ | GPragma _ | GText _ -> acc

let identities ?(extra_locs = []) ?(extra_procs = []) ?(extra_type_ids = [])
    global =
  let pids = procs_of_global global @ extra_procs |> sorted_procs in
  let nodes = InterCfg.nodesof global.Global.icfg |> sorted_nodes in
  let locs =
    locs_of_global global @ extra_locs
    |> List.sort Loc.compare
    |> uniq_sorted Loc.compare
  in
  let types =
    List.fold_left (fun acc g -> global_types g acc) [] global.Global.file.globals
    |> fun acc -> List.fold_left (fun acc loc -> loc_types loc acc) acc locs
    |> sort_by type_id
    |> uniq_sorted (fun x y -> compare (type_id x) (type_id y))
  in
  let surface_type_ids =
    type_ids_in_json [] (file global.Global.file)
    |> fun acc -> type_ids_in_json acc (icfg global.Global.icfg)
    |> fun acc -> extra_type_ids @ acc
    |> sorted_strings
  in
  let typed_type_ids = List.map type_id types |> sorted_strings in
  let extra_type_ids =
    List.filter (fun type_id -> not (List.mem type_id typed_type_ids))
      surface_type_ids
  in
  let type_id_identity type_id = assoc [("id", str type_id)] in
  assoc [
    ("procedures", list (List.map (fun pid -> assoc [("id", str pid)]) pids));
    ("nodes", list (List.map json_node nodes));
    ("locations", list (List.map loc_json locs));
    ("types", list (List.map json_type types @ List.map type_id_identity extra_type_ids));
  ]

let options_metadata () =
  assoc [
    ("optil", bool !Options.optil);
    ("bugfinder", int !Options.bugfinder);
    ("scaffold", bool !Options.scaffold);
    ("int_overflow", bool !Options.int_overflow);
    ("pfs", int !Options.pfs);
    ("narrowing", bool !Options.narrow);
    ("inline", json_string_list !Options.inline);
    ("unsound_alloc", bool !Options.unsound_alloc);
    ("unsound_loop", json_string_list (BatSet.elements !Options.unsound_loop));
    ("unsound_lib", json_string_list (BatSet.elements !Options.unsound_lib));
    ("unsound_recursion", bool !Options.unsound_recursion);
    ("top_location", bool !Options.top_location);
  ]

let metadata files =
  assoc [
    ("tool", str "sparrow-oracle-dump");
    ("analysis", str "interval");
    ("configuration", str "default");
    ("files", list (List.map str files));
    ("options", options_metadata ());
  ]

let json_global global =
  assoc [
    ("file", file global.Global.file);
    ("icfg", icfg global.Global.icfg);
    ("callgraph", callgraph global);
    ("dump", dump global.Global.dump);
    ("mem", memory global.Global.mem);
    ("table", table global.Global.table);
  ]

let sha256_hex input =
  let int32_of_hex text =
    Int64.of_string ("0x" ^ text) |> Int64.to_int32
  in
  let right_rotate value amount =
    Int32.logor (Int32.shift_right_logical value amount)
      (Int32.shift_left value (32 - amount))
  in
  let add4 a b c d = Int32.add (Int32.add a b) (Int32.add c d) in
  let add5 a b c d e = Int32.add (add4 a b c d) e in
  let k =
    Array.map int32_of_hex
      [|
        "428a2f98"; "71374491"; "b5c0fbcf"; "e9b5dba5";
        "3956c25b"; "59f111f1"; "923f82a4"; "ab1c5ed5";
        "d807aa98"; "12835b01"; "243185be"; "550c7dc3";
        "72be5d74"; "80deb1fe"; "9bdc06a7"; "c19bf174";
        "e49b69c1"; "efbe4786"; "0fc19dc6"; "240ca1cc";
        "2de92c6f"; "4a7484aa"; "5cb0a9dc"; "76f988da";
        "983e5152"; "a831c66d"; "b00327c8"; "bf597fc7";
        "c6e00bf3"; "d5a79147"; "06ca6351"; "14292967";
        "27b70a85"; "2e1b2138"; "4d2c6dfc"; "53380d13";
        "650a7354"; "766a0abb"; "81c2c92e"; "92722c85";
        "a2bfe8a1"; "a81a664b"; "c24b8b70"; "c76c51a3";
        "d192e819"; "d6990624"; "f40e3585"; "106aa070";
        "19a4c116"; "1e376c08"; "2748774c"; "34b0bcb5";
        "391c0cb3"; "4ed8aa4a"; "5b9cca4f"; "682e6ff3";
        "748f82ee"; "78a5636f"; "84c87814"; "8cc70208";
        "90befffa"; "a4506ceb"; "bef9a3f7"; "c67178f2";
      |]
  in
  let h =
    Array.map int32_of_hex
      [|
        "6a09e667"; "bb67ae85"; "3c6ef372"; "a54ff53a";
        "510e527f"; "9b05688c"; "1f83d9ab"; "5be0cd19";
      |]
  in
  let length = String.length input in
  let padded_length =
    let min_length = length + 1 + 8 in
    ((min_length + 63) / 64) * 64
  in
  let padded = Bytes.make padded_length '\000' in
  Bytes.blit_string input 0 padded 0 length;
  Bytes.set padded length (Char.chr 0x80);
  let bit_length = Int64.mul (Int64.of_int length) 8L in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte =
      Int64.shift_right_logical bit_length shift
      |> Int64.logand 0xffL
      |> Int64.to_int
    in
    Bytes.set padded (padded_length - 8 + index) (Char.chr byte)
  done;
  let read_word offset =
    let byte index =
      Bytes.get padded (offset + index) |> Char.code |> Int32.of_int
    in
    Int32.logor
      (Int32.shift_left (byte 0) 24)
      (Int32.logor
         (Int32.shift_left (byte 1) 16)
         (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3)))
  in
  for chunk = 0 to (padded_length / 64) - 1 do
    let offset = chunk * 64 in
    let w = Array.make 64 0l in
    for index = 0 to 15 do
      w.(index) <- read_word (offset + (index * 4))
    done;
    for index = 16 to 63 do
      let s0 =
        Int32.logxor
          (Int32.logxor (right_rotate w.(index - 15) 7)
             (right_rotate w.(index - 15) 18))
          (Int32.shift_right_logical w.(index - 15) 3)
      in
      let s1 =
        Int32.logxor
          (Int32.logxor (right_rotate w.(index - 2) 17)
             (right_rotate w.(index - 2) 19))
          (Int32.shift_right_logical w.(index - 2) 10)
      in
      w.(index) <- add4 w.(index - 16) s0 w.(index - 7) s1
    done;
    let a = ref h.(0) in
    let b = ref h.(1) in
    let c = ref h.(2) in
    let d = ref h.(3) in
    let e = ref h.(4) in
    let f = ref h.(5) in
    let g = ref h.(6) in
    let h_work = ref h.(7) in
    for index = 0 to 63 do
      let s1 =
        Int32.logxor
          (Int32.logxor (right_rotate !e 6) (right_rotate !e 11))
          (right_rotate !e 25)
      in
      let ch =
        Int32.logxor (Int32.logand !e !f)
          (Int32.logand (Int32.lognot !e) !g)
      in
      let temp1 = add5 !h_work s1 ch k.(index) w.(index) in
      let s0 =
        Int32.logxor
          (Int32.logxor (right_rotate !a 2) (right_rotate !a 13))
          (right_rotate !a 22)
      in
      let maj =
        Int32.logxor
          (Int32.logxor (Int32.logand !a !b) (Int32.logand !a !c))
          (Int32.logand !b !c)
      in
      let temp2 = Int32.add s0 maj in
      h_work := !g;
      g := !f;
      f := !e;
      e := Int32.add !d temp1;
      d := !c;
      c := !b;
      b := !a;
      a := Int32.add temp1 temp2
    done;
    h.(0) <- Int32.add h.(0) !a;
    h.(1) <- Int32.add h.(1) !b;
    h.(2) <- Int32.add h.(2) !c;
    h.(3) <- Int32.add h.(3) !d;
    h.(4) <- Int32.add h.(4) !e;
    h.(5) <- Int32.add h.(5) !f;
    h.(6) <- Int32.add h.(6) !g;
    h.(7) <- Int32.add h.(7) !h_work
  done;
  let hex_digit value =
    String.get "0123456789abcdef" value
  in
  let buffer = Buffer.create 64 in
  let add_word word =
    List.iter
      (fun shift ->
         Int32.shift_right_logical word shift
         |> Int32.logand 0xfl
         |> Int32.to_int
         |> hex_digit
         |> Buffer.add_char buffer)
      [28; 24; 20; 16; 12; 8; 4; 0]
  in
  Array.iter add_word h;
  Buffer.contents buffer

let starts_with prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length
  && String.sub value 0 prefix_length = prefix

let is_type_object fields =
  List.exists
    (function
      | "id", `String value -> starts_with "type:" value
      | _ -> false)
    fields

let is_source_location_json = function
  | `Assoc fields ->
    List.mem_assoc "file" fields
    && (List.mem_assoc "line" fields || List.mem_assoc "display" fields)
  | _ -> false

let rec normalize_json = function
  | `Assoc _ as object_json when is_source_location_json object_json ->
    `Null
  | `Assoc fields ->
    let type_object = is_type_object fields in
    fields
    |> List.filter (fun (key, _) ->
        key <> "pretty"
        && key <> "display"
        && not (type_object && key = "text"))
    |> List.filter (fun (_, value) -> not (is_source_location_json value))
    |> List.map (fun (key, value) -> (key, normalize_json value))
    |> List.sort (fun (left, _) (right, _) -> compare left right)
    |> fun fields -> `Assoc fields
  | `List values -> `List (List.map normalize_json values)
  | json -> json

let json_digest json =
  json |> normalize_json |> Yojson.Safe.to_string |> sha256_hex

let json_sort_key json =
  json |> normalize_json |> Yojson.Safe.to_string

let compare_json left right =
  String.compare (json_sort_key left) (json_sort_key right)

let sort_json_values values =
  List.sort compare_json values

let string_key key = function
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some (`String value) -> Some value
     | _ -> None)
  | _ -> None

let compare_by_optional_string key left right =
  match string_key key left, string_key key right with
  | Some left_key, Some right_key ->
    let cmp = String.compare left_key right_key in
    if cmp <> 0 then cmp else compare_json left right
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> compare_json left right

let sort_json_by_string key values =
  List.sort (compare_by_optional_string key) values

let compare_global_item left right =
  match left, right with
  | `Assoc left_fields, `Assoc right_fields ->
    (match List.assoc_opt "order" left_fields,
           List.assoc_opt "order" right_fields with
     | Some (`Int left_order), Some (`Int right_order) ->
       let cmp = compare left_order right_order in
       if cmp <> 0 then cmp else compare_json left right
     | Some (`Int _), _ -> -1
     | _, Some (`Int _) -> 1
     | _ -> compare_json left right)
  | _ -> compare_json left right

let replace_list_field key sort_values fields =
  List.map
    (fun (field, value) ->
       if field = key then
         match value with
         | `List values -> field, `List (sort_values values)
         | _ -> field, value
       else field, value)
    fields

let canonical_global_file_json = function
  | `Assoc fields ->
    fields
    |> replace_list_field "globals"
      (fun values ->
         values
         |> List.map normalize_json
         |> List.sort compare_global_item)
    |> assoc
    |> normalize_json
  | json -> normalize_json json

let canonical_icfg_cfg_json = function
  | `Assoc fields ->
    fields
    |> replace_list_field "nodes"
      (fun values ->
         values
         |> List.map normalize_json
         |> sort_json_by_string "id")
    |> replace_list_field "edges"
      (fun values ->
         values
         |> List.map normalize_json
         |> sort_json_values)
    |> assoc
    |> normalize_json
  | json -> normalize_json json

let canonical_global_icfg_json = function
  | `Assoc fields ->
    fields
    |> replace_list_field "procedures"
      (fun values ->
         values
         |> List.map normalize_json
         |> sort_json_by_string "id")
    |> replace_list_field "cfgs"
      (fun values ->
         values
         |> List.map canonical_icfg_cfg_json
         |> sort_json_by_string "procedure")
    |> replace_list_field "call_edges"
      (fun values ->
         values
         |> List.map normalize_json
         |> sort_json_values)
    |> assoc
    |> normalize_json
  | json -> normalize_json json

let canonical_callgraph_json = function
  | `Assoc fields ->
    fields
    |> replace_list_field "nodes"
      (fun values ->
         values
         |> List.map normalize_json
         |> sort_json_values)
    |> replace_list_field "direct_edges"
      (fun values ->
         values
         |> List.map normalize_json
         |> sort_json_values)
    |> replace_list_field "transitive_edges"
      (fun values ->
         values
         |> List.map normalize_json
         |> sort_json_values)
    |> assoc
    |> normalize_json
  | json -> normalize_json json

let string_list_json _path json =
  let values =
    match json with
    | `List values ->
      List.filter_map
        (function
          | `String value -> Some value
          | _ -> None)
        values
    | _ -> []
  in
  values
  |> sorted_strings
  |> List.map str
  |> list

let canonical_interval_json json =
  normalize_json json

let canonical_array_json = function
  | `Assoc fields ->
    (match List.assoc_opt "kind" fields with
     | Some (`String "top") -> assoc [("kind", str "top")]
     | _ ->
       let entries =
         match List.assoc_opt "entries" fields with
         | Some (`List entries) ->
           entries
           |> List.filter_map (function
               | `Assoc entry_fields ->
                 (match List.assoc_opt "allocsite" entry_fields with
                  | Some (`String allocsite) ->
                    Some
                      ( allocsite,
                        assoc [
                          ("allocsite", str allocsite);
                          ( "offset",
                            match List.assoc_opt "offset" entry_fields with
                            | Some value -> canonical_interval_json value
                            | None -> `Null );
                          ( "size",
                            match List.assoc_opt "size" entry_fields with
                            | Some value -> canonical_interval_json value
                            | None -> `Null );
                          ( "stride",
                            match List.assoc_opt "stride" entry_fields with
                            | Some value -> canonical_interval_json value
                            | None -> `Null );
                          ( "null_pos",
                            match List.assoc_opt "null_pos" entry_fields with
                            | Some value -> canonical_interval_json value
                            | None -> `Null );
                          ( "structure",
                            match List.assoc_opt "structure" entry_fields with
                            | Some value ->
                              string_list_json
                                "$.value.array.entries[].structure" value
                            | None -> list [] );
                        ] )
                  | _ -> None)
               | _ -> None)
           |> List.sort (fun (left, _) (right, _) -> String.compare left right)
           |> List.map snd
         | _ -> []
       in
       assoc [("kind", str "map"); ("entries", list entries)])
  | _ -> assoc []

let canonical_struct_json = function
  | `Assoc fields ->
    (match List.assoc_opt "kind" fields with
     | Some (`String "top") -> assoc [("kind", str "top")]
     | _ ->
       let entries =
         match List.assoc_opt "entries" fields with
         | Some (`List entries) ->
           entries
           |> List.filter_map (function
               | `Assoc entry_fields ->
                 (match List.assoc_opt "location" entry_fields with
                  | Some (`String location) ->
                    Some
                      ( location,
                        assoc [
                          ("location", str location);
                          ( "structures",
                            match List.assoc_opt "structures" entry_fields with
                            | Some value ->
                              string_list_json
                                "$.value.struct.entries[].structures" value
                            | None -> list [] );
                        ] )
                  | _ -> None)
               | _ -> None)
           |> List.sort (fun (left, _) (right, _) -> String.compare left right)
           |> List.map snd
         | _ -> []
       in
       assoc [("kind", str "map"); ("entries", list entries)])
  | _ -> assoc []

let canonical_value_json = function
  | `Assoc fields ->
    let field key =
      match List.assoc_opt key fields with
      | Some value -> value
      | None -> `Null
    in
    assoc [
      ("interval", canonical_interval_json (field "interval"));
      ("points_to", string_list_json "$.value.points_to" (field "points_to"));
      ("array", canonical_array_json (field "array"));
      ("struct", canonical_struct_json (field "struct"));
      ("procedures", string_list_json "$.value.procedures" (field "procedures"));
    ]
  | _ -> assoc []

let canonical_memory_json mem_json =
  let entries =
    match mem_json with
    | `List entries ->
      entries
      |> List.filter_map (function
          | `Assoc fields ->
            (match List.assoc_opt "location" fields,
                   List.assoc_opt "value" fields
             with
             | Some (`String location), Some value ->
               Some (location, canonical_value_json value)
             | _ -> None)
          | _ -> None)
      |> List.sort (fun (left, _) (right, _) -> String.compare left right)
      |> List.map (fun (location, value) ->
          assoc [("location", str location); ("value", value)])
    | _ -> []
  in
  list entries

let post_pre_global_surface global =
  assoc [
    ("file", file global.Global.file);
    ("icfg", icfg global.Global.icfg);
    ("callgraph", callgraph global);
    ("mem", memory global.Global.mem);
  ]

let post_pre_global_fingerprint_surface global =
  assoc [
    ("file", canonical_global_file_json (file global.Global.file));
    ("icfg", canonical_global_icfg_json (icfg global.Global.icfg));
    ("callgraph", canonical_callgraph_json (callgraph global));
    ("mem", canonical_memory_json (memory global.Global.mem));
  ]

let linking_identity files global =
  let proc_order =
    InterCfg.pidsof global.Global.icfg
    |> sorted_procs
    |> List.map str
    |> list
  in
  let surface = post_pre_global_fingerprint_surface global in
  let run_surface =
    assoc [
      ("files", list (List.map str files));
      ("proc_order", proc_order);
      ("surface", surface);
    ]
  in
  assoc [
    ("run_id", str ("oracle:" ^ json_digest run_surface));
    ("proc_order", proc_order);
    ("surface_hash", str (json_digest surface));
    ("digest_algorithm", str "sha256");
  ]

let to_json stage files global =
  assoc [
    ("schema", str "sparrow.oracle.v1");
    ("stage", str (string_of_stage stage));
    ("metadata", metadata files);
    ("identities", identities global);
    ("global", json_global global);
  ]

(* Modules for sparse analysis intermediate structures *)
module MyAccessSem = AccessSem.Make(ItvSem)
module MyAccessAnalysis = AccessAnalysis.Make(MyAccessSem)
module MyDUGraph = Dug.Make(ItvDom.Mem)
module MySsaDug = SsaDug.Make(MyDUGraph)(MyAccessAnalysis.Access)
module MyWorklist = Worklist.Make(MyDUGraph)

let reset_sparse_caches () =
  ItvAnalysis.Analysis.clear_cache ();
  MySsaDug.clear_cache ()

let loc_id_string_list locs =
  locs |> List.map loc_id |> sorted_strings

let json_of_loc_list locs =
  loc_id_string_list locs |> List.map str |> list

let json_of_locset locs =
  PowLoc.elements locs |> json_of_loc_list

let json_of_nodes nodes =
  nodes
  |> List.sort InterCfg.Node.compare
  |> List.map node_id
  |> List.map str
  |> list

let json_of_pownode nodes =
  PowNode.elements nodes |> json_of_nodes

let json_of_access_info info =
  let use_locs = MyAccessAnalysis.Access.Info.useof info in
  let def_locs = MyAccessAnalysis.Access.Info.defof info in
  assoc [
    ("use", json_of_locset use_locs);
    ("def", json_of_locset def_locs);
    ("all", json_of_locset (PowLoc.union use_locs def_locs));
  ]

let json_of_access global access =
  let node_entries =
    MyAccessAnalysis.Access.fold (fun node info acc ->
      (node, info) :: acc
    ) access []
    |> List.sort (fun (n1, _) (n2, _) -> InterCfg.Node.compare n1 n2)
  in
  let by_node =
    node_entries
    |> List.map (fun (node, info) -> (node_id node, json_of_access_info info))
    |> assoc
  in
  let nodes =
    node_entries
    |> List.map (fun (node, info) ->
      assoc [
        ("node", str (node_id node));
        ("procedure", str (InterCfg.Node.get_pid node));
        ("use", json_of_locset (MyAccessAnalysis.Access.Info.useof info));
        ("def", json_of_locset (MyAccessAnalysis.Access.Info.defof info));
      ])
    |> list
  in
  let procedure_entries =
    InterCfg.pidsof global.Global.icfg
    |> sorted_procs
    |> List.map (fun pid ->
      (pid, assoc [
        ("direct", json_of_access_info (MyAccessAnalysis.Access.find_proc pid access));
        ("reachable", json_of_access_info (MyAccessAnalysis.Access.find_proc_reach pid access));
        ("reachable_without_local",
          json_of_access_info (MyAccessAnalysis.Access.find_proc_reach_wo_local pid access));
        ("local", json_of_locset (MyAccessAnalysis.Access.find_proc_local pid access));
      ]))
  in
  let by_procedure =
    procedure_entries
    |> List.map (fun (pid, info) -> (pid, info))
    |> assoc
  in
  let procedures =
    procedure_entries
    |> List.map (fun (pid, info) ->
      assoc [
        ("procedure", str pid);
        ("access", info);
      ])
    |> list
  in
  let loc_node_entries find =
    MyAccessAnalysis.Access.total_abslocs access
    |> PowLoc.elements
    |> List.sort Loc.compare
    |> List.map (fun loc ->
      assoc [
        ("location", str (loc_id loc));
        ("nodes", json_of_pownode (find loc access));
      ])
    |> list
  in
  assoc [
    ("by_node", by_node);
    ("by_procedure", by_procedure);
    ("nodes", nodes);
    ("procedures", procedures);
    ("total_locations", json_of_locset (MyAccessAnalysis.Access.total_abslocs access));
    ("def_nodes", loc_node_entries MyAccessAnalysis.Access.find_def_nodes);
    ("use_nodes", loc_node_entries MyAccessAnalysis.Access.find_use_nodes);
  ]

let dug_edge_kind global src dst =
  let src_pid = InterCfg.Node.get_pid src in
  let dst_pid = InterCfg.Node.get_pid dst in
  if InterCfg.is_callnode src global.Global.icfg
     && IntraCfg.is_entry (InterCfg.Node.get_cfgnode dst) then
    "call"
  else if IntraCfg.is_exit (InterCfg.Node.get_cfgnode src)
       && InterCfg.is_returnnode dst global.Global.icfg then
    "return"
  else if src_pid = dst_pid then "intra"
  else "inter"

let json_of_dug global dug =
  let nodes =
    MyDUGraph.fold_node (fun v acc -> v :: acc) dug []
    |> List.sort InterCfg.Node.compare
  in
  let edges =
    MyDUGraph.fold_edges (fun src dst acc ->
      let locs = MyDUGraph.get_abslocs src dst dug in
      let edge = assoc [
        ("src", str (node_id src));
        ("dst", str (node_id dst));
        ("src_procedure", str (InterCfg.Node.get_pid src));
        ("dst_procedure", str (InterCfg.Node.get_pid dst));
        ("kind", str (dug_edge_kind global src dst));
        ("locs", json_of_locset locs);
      ] in
      (node_id src, node_id dst, edge) :: acc
    ) dug []
    |> List.sort (fun (src1, dst1, _) (src2, dst2, _) ->
      let cmp = String.compare src1 src2 in
      if cmp <> 0 then cmp else String.compare dst1 dst2)
    |> List.map (fun (_, _, edge) -> edge)
  in
  assoc [
    ("nodes", json_of_nodes nodes);
    ("edges", list edges);
    ("node_count", int (List.length nodes));
    ("edge_count", int (List.length edges));
    ("label_count", int (MyDUGraph.nb_loc dug));
  ]

let json_of_worklist_info worklist =
  let snapshot = MyWorklist.snapshot worklist in
  let order =
    snapshot.MyWorklist.order
    |> List.map (fun entry ->
      assoc [
        ("node", str (node_id entry.MyWorklist.node));
        ("order", int entry.MyWorklist.order);
        ("loop_header", bool entry.MyWorklist.loop_header);
        ("head_order", opt int entry.MyWorklist.head_order);
      ])
  in
  let scc_ids nodes =
    nodes
    |> List.map node_id
    |> sorted_strings
    |> List.map str
    |> list
  in
  let scc_order =
    snapshot.MyWorklist.sccs |> List.map scc_ids |> list
  in
  let sccs =
    snapshot.MyWorklist.sccs
    |> List.mapi (fun index nodes ->
      assoc [
        ("index", int index);
        ("nodes", scc_ids nodes);
      ])
    |> list
  in
  let loop_headers =
    snapshot.MyWorklist.loop_headers
    |> List.map node_id
    |> sorted_strings
    |> List.map str
    |> list
  in
  assoc [
    ("order", list order);
    ("scc_order", scc_order);
    ("sccs", sccs);
    ("loop_headers", loop_headers);
  ]

let sparse_spec_json locset locset_fs premem unsound_lib unsound_update unsound_bitwise =
  assoc [
    ("locsets", assoc [
      ("all", json_of_locset locset);
      ("flow_sensitive", json_of_locset locset_fs);
    ]);
    ("premem", memory premem);
    ("ptrinfo", table ItvDom.Table.empty);
    ("unsound_lib", json_string_list (BatSet.elements unsound_lib));
    ("unsound_update", bool unsound_update);
    ("unsound_bitwise", bool unsound_bitwise);
  ]

let collect_access_locs access acc =
  PowLoc.elements (MyAccessAnalysis.Access.total_abslocs access) @ acc

let collect_dug_locs dug acc =
  MyDUGraph.fold_edges (fun src dst acc ->
    PowLoc.elements (MyDUGraph.get_abslocs src dst dug) @ acc
  ) dug acc

let sparse_identity_locs inputof outputof access dug locset locset_fs =
  []
  |> collect_table_locs inputof
  |> collect_table_locs outputof
  |> collect_access_locs access
  |> collect_dug_locs dug
  |> fun acc -> PowLoc.elements locset @ PowLoc.elements locset_fs @ acc

let sparse_identity_procs inputof outputof premem =
  []
  |> collect_table_procs inputof
  |> collect_table_procs outputof
  |> collect_mem_procs premem

let to_json_sparse files pre_global global inputof outputof access dug worklist
    locset locset_fs premem unsound_lib unsound_update unsound_bitwise =
  let locset_json = json_of_locset locset in
  let locset_fs_json = json_of_locset locset_fs in
  let post_pre_global_json = post_pre_global_surface pre_global in
  let sparse_json = assoc [
    ("locsets", assoc [
      ("all", locset_json);
      ("flow_sensitive", locset_fs_json);
    ]);
    ("locset", locset_json);
    ("locset_fs", locset_fs_json);
    ("spec", sparse_spec_json locset locset_fs premem unsound_lib unsound_update unsound_bitwise);
    ("access", json_of_access global access);
    ("dug", json_of_dug global dug);
    ("worklist", json_of_worklist_info worklist);
    ("post_pre_global", post_pre_global_json);
    ("post_pre_global_fingerprint", linking_identity files pre_global);
    ("callgraph", callgraph global);
    ("dump", dump global.Global.dump);
  ] in
  let extra_locs =
    sparse_identity_locs inputof outputof access dug locset locset_fs
  in
  assoc [
    ("schema", str "sparrow.oracle.v1");
    ("stage", str (string_of_stage Sparse));
    ("metadata", metadata files);
    ( "identities",
      identities ~extra_locs
        ~extra_type_ids:(type_ids_in_json [] post_pre_global_json)
        ~extra_procs:
          (procs_of_global pre_global
           @ sparse_identity_procs inputof outputof pre_global.Global.mem)
        global );
    ("global", json_global global);
    ("inputof", table inputof);
    ("outputof", table outputof);
    ("sparse", sparse_json);
  ]

let write_sparse path files global =
  reset_sparse_caches ();
  let (global_anal, inputof, outputof, _) = ItvAnalysis.do_analysis global in
  let locset = ItvAnalysis.get_locset global.Global.mem in
  let locset_fs = PartialFlowSensitivity.select global locset in
  let unsound_lib = UnsoundLib.collect global in
  let unsound_update = (!Options.bugfinder >= 2) in
  let unsound_bitwise = (!Options.bugfinder >= 1) in
  let spec = { ItvSem.Spec.empty with
    ItvSem.Spec.locset; locset_fs; premem = global.Global.mem;
    ItvSem.Spec.unsound_lib;
    unsound_update;
    unsound_bitwise;
  } in
  let access = MyAccessAnalysis.perform global locset (ItvSem.run AbsSem.Strong spec) global.Global.mem in
  let dug = MySsaDug.make (global, access, locset_fs) in
  let worklist = MyWorklist.init dug in
  let chan = open_out path in
  try
    to_json_sparse files global global_anal inputof outputof access dug worklist
      locset locset_fs global.Global.mem unsound_lib unsound_update
      unsound_bitwise
    |> Yojson.Safe.pretty_to_channel chan;
    output_char chan '\n';
    close_out chan
  with exn ->
    close_out_noerr chan;
    raise exn

let write path stage files global =
  let chan = open_out path in
  try
    to_json stage files global
    |> Yojson.Safe.pretty_to_channel chan;
    output_char chan '\n';
    close_out chan
  with exn ->
    close_out_noerr chan;
    raise exn
