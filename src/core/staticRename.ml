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
(** Front-end normalization pass (separate-compilation A1, P-2):
    qualify every file-static symbol of a translation unit as
    [name@<module_key>], where [module_key] is derived from the TU's
    source path.  Gated by [Options.static_rename]; with the flag off this
    module is never invoked and the oracle pipeline is byte-for-byte
    unchanged.

    Why: C's [static] linkage makes same-named privates in different TUs
    distinct objects, but the analyzer's identities are name-keyed
    (procedure id = function name, abstract location GVar = global name).
    Applied per TU BEFORE any merge/link, this pass makes those identities
    globally unique by construction -- the same rename whether the TU is
    compiled alone (module object) or fed to a whole-program oracle run
    (oracle-aligned mode), which is what makes the two node-key comparable.

    What is renamed: exactly the globals with [vstorage = Static]
    (file-static functions, file-static globals, and CIL-hoisted
    function-local statics -- CIL moves those to file scope with
    alpha-uniqued names before we run).  References need no rewriting:
    within one parsed TU all occurrences of a global share one [varinfo],
    so mutating [vname] in place renames every use, including the
    function's own pid read from [fd.svar.vname] at CFG construction.

    The [@] separator cannot occur in a C identifier, so a renamed symbol
    can never collide with a real one, and stripping the qualifier for
    reports is unambiguous. *)

open Sparrow_cil

(* Module key derived from the TU path AS GIVEN on the command line
   (leading "./" segments dropped, extension dropped, remaining chars
   outside [A-Za-z0-9_] mapped to '_').  The path -- not just the basename
   -- so that TUs of the same name in different directories get distinct
   keys; invocations that must be key-compatible (module compile vs
   oracle-aligned run) must therefore pass path-identical arguments, which
   the harness scripts do.  Keys are checked pairwise-distinct per parse
   ([assert_distinct_keys]); a collision (e.g. "a/b.i" vs "a_b.i" both
   sanitizing to "a_b") is a loud error, never a silent re-collision of
   statics. *)
let module_key_of_path : string -> string
= fun path ->
  let rec drop_dot_slash s =
    if String.length s > 2 && String.sub s 0 2 = "./"
    then drop_dot_slash (String.sub s 2 (String.length s - 2))
    else s
  in
  let path = drop_dot_slash path in
  let without_ext =
    try Filename.remove_extension path with Invalid_argument _ -> path
  in
  let sanitize c =
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> c
    | _ -> '_'
  in
  String.map sanitize without_ext

let qualify : key:string -> string -> string
= fun ~key name -> name ^ "@" ^ key

(* Loud pairwise key-distinctness check over the paths of one parse. *)
let assert_distinct_keys : string list -> unit
= fun paths ->
  let tbl = Hashtbl.create 17 in
  List.iter (fun path ->
      let key = module_key_of_path path in
      match Hashtbl.find_opt tbl key with
      | Some other when other <> path ->
        prerr_endline
          ("Error: -static_rename module-key collision: '" ^ path ^ "' and '"
           ^ other ^ "' both derive module key '" ^ key ^ "'");
        exit 1
      | _ -> Hashtbl.replace tbl key path)
    paths

(* Rename the file-statics of [file] (parsed from [path]) in place.
   Idempotence per varinfo (a static function appears as both GVarDecl
   prototype and GFun definition sharing one varinfo) is ensured by
   remembering visited vids -- unique within one parsed TU. *)
let rename_file : path:string -> Sparrow_cil.file -> Sparrow_cil.file
= fun ~path file ->
  let key = module_key_of_path path in
  let seen : (int, unit) Hashtbl.t = Hashtbl.create 64 in
  let rename_varinfo vi =
    if vi.vstorage = Static && vi.vglob && not (Hashtbl.mem seen vi.vid) then
      begin
        Hashtbl.add seen vi.vid ();
        vi.vname <- qualify ~key vi.vname
      end
  in
  iterGlobals file (fun glob ->
      match glob with
      | GFun (fd, _) -> rename_varinfo fd.svar
      | GVar (vi, _, _) | GVarDecl (vi, _) -> rename_varinfo vi
      | _ -> ());
  file
