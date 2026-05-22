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

(* This is a smoke test for the oracle dump path. It checks that the
   executable runs, produces byte-identical repeated dumps, and exposes the
   minimum v1 JSON shape. It is not a semantic proof of oracle equality. *)

let fail msg =
  prerr_endline ("oracle_dump_smoke: " ^ msg);
  exit 1

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let status_to_string = function
  | Unix.WEXITED code -> "exited " ^ string_of_int code
  | Unix.WSIGNALED signal -> "signaled " ^ string_of_int signal
  | Unix.WSTOPPED signal -> "stopped " ^ string_of_int signal

let run_with_redirects exe argv stdout_path stderr_path =
  let stdout_fd =
    Unix.openfile stdout_path [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC] 0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC] 0o600
  in
  let pid =
    try Unix.create_process exe argv Unix.stdin stdout_fd stderr_fd
    with exn ->
      Unix.close stdout_fd;
      Unix.close stderr_fd;
      raise exn
  in
  Unix.close stdout_fd;
  Unix.close stderr_fd;
  snd (Unix.waitpid [] pid)

let run_oracle tmp_dir oracle_exe source stage index =
  let stem = stage ^ string_of_int index in
  let out = Filename.concat tmp_dir (stem ^ ".json") in
  let stdout_path = Filename.concat tmp_dir (stem ^ ".stdout") in
  let stderr_path = Filename.concat tmp_dir (stem ^ ".stderr") in
  let argv =
    [| oracle_exe; "--stage"; stage; "--out"; out; source |]
  in
  match run_with_redirects oracle_exe argv stdout_path stderr_path with
  | Unix.WEXITED 0 -> out
  | status ->
    fail
      (stage ^ " oracle dump " ^ status_to_string status
       ^ "\nstdout:\n" ^ read_file stdout_path
       ^ "\nstderr:\n" ^ read_file stderr_path)

let run_cmp left right =
  let dev_null = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o600 in
  let status =
    try
      let pid =
        Unix.create_process "cmp" [| "cmp"; "-s"; left; right |]
          Unix.stdin dev_null dev_null
      in
      Unix.close dev_null;
      snd (Unix.waitpid [] pid)
    with exn ->
      Unix.close dev_null;
      raise exn
  in
  match status with
  | Unix.WEXITED 0 -> ()
  | _ -> fail ("non-deterministic oracle dump: " ^ left ^ " differs from " ^ right)

let assoc_opt key fields =
  let rec loop = function
    | [] -> None
    | (k, v) :: rest -> if k = key then Some v else loop rest
  in
  loop fields

let require_field path key = function
  | `Assoc fields ->
    (match assoc_opt key fields with
     | Some value -> value
     | None -> fail ("missing JSON field " ^ path ^ "." ^ key))
  | _ -> fail ("expected JSON object at " ^ path)

let require_string path expected = function
  | `String actual when actual = expected -> ()
  | `String actual ->
    fail ("unexpected " ^ path ^ ": expected " ^ expected ^ ", got " ^ actual)
  | _ -> fail ("expected JSON string at " ^ path)

let require_list path = function
  | `List _ -> ()
  | _ -> fail ("expected JSON list at " ^ path)

let require_object path = function
  | `Assoc _ -> ()
  | _ -> fail ("expected JSON object at " ^ path)

let check_common_shape expected_stage json =
  let schema = require_field "$" "schema" json in
  let stage = require_field "$" "stage" json in
  let metadata = require_field "$" "metadata" json in
  let identities = require_field "$" "identities" json in
  let global = require_field "$" "global" json in
  require_string "$.schema" "sparrow.oracle.v1" schema;
  require_string "$.stage" expected_stage stage;
  require_list "$.metadata.files" (require_field "$.metadata" "files" metadata);
  require_list "$.identities.procedures"
    (require_field "$.identities" "procedures" identities);
  require_list "$.identities.nodes"
    (require_field "$.identities" "nodes" identities);
  require_object "$.global.file" (require_field "$.global" "file" global);
  require_object "$.global.icfg" (require_field "$.global" "icfg" global);
  global

let check_pre_shape global =
  require_list "$.global.mem" (require_field "$.global" "mem" global);
  require_list "$.global.table" (require_field "$.global" "table" global);
  require_object "$.global.callgraph" (require_field "$.global" "callgraph" global);
  require_list "$.global.dump" (require_field "$.global" "dump" global)

let check_sparse_shape json global =
  check_pre_shape global;
  require_list "$.inputof" (require_field "$" "inputof" json);
  require_list "$.outputof" (require_field "$" "outputof" json);
  let sparse = require_field "$" "sparse" json in
  let locsets = require_field "$.sparse" "locsets" sparse in
  require_object "$.sparse.locsets" locsets;
  require_list "$.sparse.locsets.all"
    (require_field "$.sparse.locsets" "all" locsets);
  require_list "$.sparse.locsets.flow_sensitive"
    (require_field "$.sparse.locsets" "flow_sensitive" locsets);
  require_list "$.sparse.locset" (require_field "$.sparse" "locset" sparse);
  require_list "$.sparse.locset_fs"
    (require_field "$.sparse" "locset_fs" sparse);
  require_object "$.sparse.access"
    (require_field "$.sparse" "access" sparse);
  let dug = require_field "$.sparse" "dug" sparse in
  require_object "$.sparse.dug" dug;
  require_list "$.sparse.dug.nodes"
    (require_field "$.sparse.dug" "nodes" dug);
  require_list "$.sparse.dug.edges"
    (require_field "$.sparse.dug" "edges" dug);
  let worklist = require_field "$.sparse" "worklist" sparse in
  require_object "$.sparse.worklist" worklist;
  require_list "$.sparse.worklist.order"
    (require_field "$.sparse.worklist" "order" worklist);
  require_list "$.sparse.worklist.scc_order"
    (require_field "$.sparse.worklist" "scc_order" worklist);
  require_list "$.sparse.worklist.loop_headers"
    (require_field "$.sparse.worklist" "loop_headers" worklist);
  require_object "$.sparse.callgraph"
    (require_field "$.sparse" "callgraph" sparse);
  require_list "$.sparse.dump" (require_field "$.sparse" "dump" sparse)

let check_shape stage path =
  let json = Yojson.Safe.from_file path in
  let global = check_common_shape stage json in
  if stage = "pre" then check_pre_shape global
  else if stage = "sparse" then check_sparse_shape json global

let cleanup_dir dir =
  Sys.readdir dir
  |> Array.iter (fun name -> Unix.unlink (Filename.concat dir name));
  Unix.rmdir dir

let main () =
  if Array.length Sys.argv < 2 then
    fail "usage: oracle_dump_smoke.exe ORACLE_DUMP_EXE [SOURCE_C_FILE]";
  let oracle_exe = Sys.argv.(1) in
  let source = if Array.length Sys.argv > 2 then Sys.argv.(2) else "test.c" in
  let tmp_dir =
    Filename.concat (Filename.get_temp_dir_name ())
      ("sparrow-oracle-smoke-" ^ string_of_int (Unix.getpid ()))
  in
  Unix.mkdir tmp_dir 0o700;
  let front1 = run_oracle tmp_dir oracle_exe source "front_end" 1 in
  let front2 = run_oracle tmp_dir oracle_exe source "front_end" 2 in
  run_cmp front1 front2;
  check_shape "front_end" front1;
  let pre1 = run_oracle tmp_dir oracle_exe source "pre" 1 in
  let pre2 = run_oracle tmp_dir oracle_exe source "pre" 2 in
  run_cmp pre1 pre2;
  check_shape "pre" pre1;
  let sparse1 = run_oracle tmp_dir oracle_exe source "sparse" 1 in
  let sparse2 = run_oracle tmp_dir oracle_exe source "sparse" 2 in
  run_cmp sparse1 sparse2;
  check_shape "sparse" sparse1;
  cleanup_dir tmp_dir;
  print_endline "oracle_dump_smoke.....PASS"

let _ =
  try main () with
  | Failure msg -> fail msg
  | exn -> fail (Printexc.to_string exn)
