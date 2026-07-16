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
open Sparrow_cil
open Vocab
open Global

module C = Sparrow_cil
module F = Frontc
module E = Errormsg

let files = ref []
let marshal_file = ref ""

let args : string -> unit
= fun f ->
  if Sys.file_exists f then
    if Filename.check_suffix f ".i" ||
    Filename.check_suffix f ".c" then
      files := f :: !files
    else
      let _ = prerr_endline ("Error: " ^ f ^ ": Not a preprocessed C") in
      exit 1
  else
    let _ = prerr_endline ("Error: " ^ f ^ ": No such file") in
    exit 1

let parseOneFile : string -> C.file
= fun fname ->
  (* PARSE and convert to CIL *)
  if !Cilutil.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  if not (Feature.enabled "epicenter") then (
    (Rmtmps.removeUnusedTemps cil)
  );
  cil

(* Parse one TU and, under -static_rename, qualify its file-statics as
   name@<module_key(path)> (A1 front-end normalization).  The rename runs
   per TU BEFORE any merge so the whole-program (oracle-aligned) run and a
   module-alone build assign the SAME names -- and mergecil then sees no
   static collisions to rename on its own. *)
let parse_one_normalized : string -> C.file
= fun fname ->
  let cil = parseOneFile fname in
  if !Options.static_rename then StaticRename.rename_file ~path:fname cil
  else cil

let parse : unit -> C.file
= fun () ->
  if !Options.static_rename then StaticRename.assert_distinct_keys !files;
  match List.map parse_one_normalized !files with
    [one] -> one
  | [] -> (prerr_endline "Error: No arguments are given"; exit 1)
  | files ->
    Mergecil.ignore_merge_conflicts := true;
    let merged = Stats.time "merge" (Mergecil.merge files) "merged" in
      if !E.hadErrors then
        E.s (E.error "There were errors during merging");
      merged

let makeCFGinfo : Sparrow_cil.file -> Sparrow_cil.file
=fun f ->
  ignore (Partial.calls_end_basic_blocks f) ;
  ignore (Partial.globally_unique_vids f) ;
  Sparrow_cil.iterGlobals f (fun glob -> match glob with
    Sparrow_cil.GFun(fd,_) ->
                  Sparrow_cil.prepareCFG fd ;
                  (* jc: blockinggraph depends on this "true" arg *)
                  (* -stable_node_ids (A1): number stmt sids per FUNCTION
                     (computeCFGInfo resets sid_counter when the
                     global-numbering arg is false), so a function's sids
                     -- the raw material of its IntraCfg node ids -- depend
                     on its own body only, not on its position in the
                     file/merge.  Default (flag off) keeps the pinned
                     global numbering. *)
                  ignore (Sparrow_cil.computeCFGInfo fd (not !Options.stable_node_ids))
  | _ -> ());
  f

(* Library-entry harness: if the translation unit has no [main], synthesize one
   that calls every function DEFINED in this TU with fresh uninitialized locals
   (= top) as arguments. This is the standard way to analyze a library module
   standalone (each exported function reachable from an entry with unknown
   inputs). No-op when a [main] already exists, so it never changes the
   whole-program path. Used by the modular report-equivalence validation
   (Doc/measurements) to analyze gnulib TUs that lack a main. *)
let build_main_harness : Sparrow_cil.file -> Sparrow_cil.file
= fun f ->
  let open Sparrow_cil in
  let has_main =
    foldGlobals f
      (fun acc g -> match g with
         | GFun (fd, _) when fd.svar.vname = "main" -> true
         | _ -> acc)
      false
  in
  if has_main then f
  else begin
    let main = emptyFunction "main" in
    main.svar.vtype <- TFun (intType, Some [], false, []);
    let calls =
      foldGlobals f
        (fun acc g -> match g with
           | GFun (fd, _) when fd.svar.vname <> "main" ->
             let actuals =
               List.map
                 (fun formal -> Lval (Var (makeTempVar main formal.vtype), NoOffset))
                 fd.sformals
             in
             Call (None, Lval (Var fd.svar, NoOffset), actuals, locUnknown, locUnknown)
             :: acc
           | _ -> acc)
        []
    in
    main.sbody <- mkBlock [ mkStmt (Instr (List.rev calls)) ];
    f.globals <- f.globals @ [ GFun (main, locUnknown) ];
    f
  end

(* true if the given function has variable number of arguments *)
let is_varargs : string -> Sparrow_cil.file -> bool
=fun fid file ->
  Sparrow_cil.foldGlobals file (fun b global ->
    match global with
    | GFun (fd,_) when fd.svar.vname = fid ->
        (match fd.svar.vtype with
        | TFun (_,_,b_va,_) -> b_va
        | _ -> b)
    | _ -> b
  ) false

let inline : Global.t -> bool
=fun global ->
  let f = global.file in
  let regexps = List.map (fun str -> Str.regexp (".*" ^ str ^ ".*")) !Options.inline in
  let to_inline =
    list_fold (fun global to_inline ->
      match global with
      | GFun (fd,_) when List.exists (fun regexp -> Str.string_match regexp fd.svar.vname 0) regexps ->
        fd.svar.vname :: to_inline
      | _ -> to_inline
    ) f.globals [] in
  let varargs_procs = List.filter (fun fid -> is_varargs fid f) to_inline in
  let recursive_procs = List.filter (fun fid -> Global.is_rec fid global) to_inline in
  let large_procs = List.filter (fun fid -> try List.length (InterCfg.nodes_of_pid global.icfg fid) > !Options.inline_size with _ -> false) to_inline in
  let to_exclude = varargs_procs @ recursive_procs @ large_procs in
  prerr_endline ("To inline : " ^ Vocab.string_of_list Vocab.id to_inline);
  prerr_endline ("Excluded variable-arguments functions : " ^ Vocab.string_of_list Vocab.id varargs_procs);
  prerr_endline ("Excluded recursive functions : " ^ Vocab.string_of_list Vocab.id recursive_procs);
  prerr_endline ("Excluded too large functions : " ^ Vocab.string_of_list Vocab.id large_procs);
  Inline.toinline := List.filter (fun fid -> not (List.mem fid to_exclude)) to_inline;
  Inline.doit f;
  not (!Inline.toinline = [])
