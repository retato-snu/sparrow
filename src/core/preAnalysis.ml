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
open Vocab
open Global
open BasicDom
open ItvDom

(* ***************************** *
 * Flow-insensitive pre-analysis *
 * ***************************** *)
(* number of fixpoint rounds of the last pre-analysis run (cost metric) *)
let last_pre_iters = ref 0

let onestep_transfer : Node.t list -> Mem.t * Global.t -> Mem.t * Global.t
=fun nodes (mem,global) ->
  list_fold (fun node (mem,global) ->
    ItvSem.run AbsSem.Weak ItvSem.Spec.empty node (mem,global)
  ) nodes (mem,global)

let rec fixpt : Node.t list -> int -> Mem.t * Global.t -> Mem.t * Global.t
=fun nodes k (mem,global) ->
  my_prerr_string ("\riteration : " ^ string_of_int k);
  flush stderr;
  let (mem',global') = onestep_transfer nodes (mem,global) in
  let mem' = Mem.widen mem mem' in
    if Mem.le mem' mem && Dump.le global'.dump global.dump
    then (last_pre_iters := k; my_prerr_newline (); (mem',global'))
    else fixpt nodes (k+1) (mem',global')

let callees_of : InterCfg.t -> InterCfg.Node.t -> Mem.t -> PowProc.t
= fun icfg node mem ->
  let pid = InterCfg.Node.get_pid node in
  let c = InterCfg.cmdof icfg node in
  match c with
  | IntraCfg.Cmd.Ccall (_, e, _, _) ->
    Val.pow_proc_of_val (ItvSem.eval pid e mem)
  | _ -> PowProc.bot

let draw_call_edges : InterCfg.Node.t list -> Mem.t -> Global.t -> Global.t
= fun nodes mem global ->
  let icfg =
    List.fold_left (fun icfg node ->
        if InterCfg.is_callnode node icfg then
          let callees = callees_of icfg node mem in
          PowProc.fold (InterCfg.add_call_edge node) callees icfg
        else icfg) global.icfg nodes
  in
  { global with icfg = icfg }

let draw_callgraph : Node.t list -> Mem.t -> Global.t -> Global.t
=fun nodes mem global ->
  let callgraph = List.fold_left (fun callgraph node ->
      let callees = callees_of global.icfg node mem in
      PowProc.fold (fun callee callgraph ->
        CallGraph.add_edge (InterCfg.Node.get_pid node) callee callgraph) callees callgraph)
    global.callgraph nodes
    |> CallGraph.compute_trans_calls
  in
  { global with callgraph = callgraph }

let file_of : Global.t -> Node.t -> string
= fun global n ->
  try (InterCfg.cmdof global.icfg n |> IntraCfg.Cmd.location_of).Sparrow_cil.file
  with _ -> ""

(* Flow-insensitive pre-analysis joins every node into one global memory.
   "default" sweeps all nodes then widens the whole memory once per round, so
   the traversal order of the fold (= what the round-level widening sees) is the
   only knob; "file" just groups that fold by source file (a stable sort,
   preserving intra-file order). *)
let order_nodes : Global.t -> Node.t list -> Node.t list
= fun global nodes ->
  match !Options.preanalysis_order with
  | "file" ->
    List.stable_sort (fun a b -> String.compare (file_of global a) (file_of global b)) nodes
  | _ -> nodes

(* Partition nodes by source file, in first-seen order, preserving node order
   within a file. *)
let group_by_file : Global.t -> Node.t list -> Node.t list list
= fun global nodes ->
  let nf = List.map (fun n -> (n, file_of global n)) nodes in
  let files =
    List.fold_left (fun acc (_, f) -> if List.mem f acc then acc else f :: acc) [] nf
    |> List.rev in
  List.map (fun f -> List.filter_map (fun (n, g) -> if g = f then Some n else None) nf) files

(* "module" order: stabilize each module (file) to its own local fixpoint --
   transfer that module's nodes and widen repeatedly until the global memory
   stops changing under that module -- before moving to the next module, then
   loop over modules until a full pass is globally stable. Unlike "default",
   widening is applied per module rather than once per whole-program sweep, so
   the result need not match the default fixpoint. *)
let modular_fixpt : Node.t list list -> Mem.t * Global.t -> Mem.t * Global.t
= fun groups init ->
  let sweeps = ref 0 in
  let stabilize_module mnodes (mem, global) =
    let rec loop (mem, global) =
      incr sweeps;
      let (mem', global') = onestep_transfer mnodes (mem, global) in
      let mem' = Mem.widen mem mem' in
      if Mem.le mem' mem && Dump.le global'.dump global.dump
      then (mem', global')
      else loop (mem', global')
    in
    loop (mem, global)
  in
  let rec outer (mem, global) =
    let (mem1, global1) =
      List.fold_left (fun st mnodes -> stabilize_module mnodes st) (mem, global) groups in
    my_prerr_string ("\rpre module-sweeps : " ^ string_of_int !sweeps);
    flush stderr;
    if Mem.le mem1 mem && Dump.le global1.dump global.dump
    then (last_pre_iters := !sweeps; my_prerr_newline (); (mem1, global1))
    else outer (mem1, global1)
  in
  outer init

let perform : Global.t -> Global.t
= fun global ->
  let nodes = InterCfg.nodesof global.icfg in
  let (mem, global) =
    match !Options.preanalysis_order with
    | "module" -> modular_fixpt (group_by_file global nodes) (Mem.bot, global)
    | _ -> fixpt (order_nodes global nodes) 1 (Mem.bot, global)
  in
  my_prerr_endline ("mem size : " ^ i2s (Mem.cardinal mem));
  { global with mem = mem }
  |> draw_call_edges nodes mem
  |> draw_callgraph nodes mem
  |> Global.remove_unreachable_functions
