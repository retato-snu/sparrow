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
open AbsSem
open Dug

let total_iterations = ref 0
let g_clock = ref 0.0
let l_clock = ref 0.0
(* cost metrics captured per phase for measurement / oracle dump *)
let last_widen_iters = ref 0
let last_narrow_iters = ref 0
let widen_time = ref 0.0
let narrow_time = ref 0.0
let iter_stats : (BasicDom.Node.t, int) Hashtbl.t = Hashtbl.create 1024
let unstable_stats : (BasicDom.Node.t, int) Hashtbl.t = Hashtbl.create 1024

module type S =
sig
  module Dom : InstrumentedMem.S
  module DUGraph : Dug.S
    with type Loc.t = Dom.A.t
     and type PowLoc.t = Dom.PowA.t
  module Worklist : Worklist.S with type DUGraph.t = DUGraph.t
  module Table : MapDom.CPO with type t = MapDom.MakeCPO(BasicDom.Node)(Dom).t and type A.t = BasicDom.Node.t and type B.t = Dom.t
  module Spec : Spec.S with type Dom.t = Dom.t and type Dom.A.t = Dom.A.t and type Dom.PowA.t = Dom.PowA.t
  type analysis_state = Worklist.t * Global.t * Table.t * Table.t
  val clear_cache : unit -> unit
  (* [?seed_closed] = (closed-node outputs, boundary nodes): the modular link
     COMBINE seed -- the engine pull-initialises inputof over the DUG and iterates
     only the boundary.  Omitted = the standard full run (start_node mem + fi-locs,
     empty outputof, every dug node) -- behaviour unchanged. *)
  val perform_with_scopes :
    ?seed_closed:(Table.t * BasicDom.Node.t BatSet.t) ->
    (BasicDom.Node.t -> (unit -> Dom.t * Global.t) -> Dom.t * Global.t) ->
    (Spec.t -> DUGraph.t -> DUGraph.node -> analysis_state ->
     (unit -> analysis_state) -> analysis_state) ->
    Spec.t -> Global.t -> Global.t * Table.t * Table.t
  val perform_with_transfer_scope :
    (BasicDom.Node.t -> (unit -> Dom.t * Global.t) -> Dom.t * Global.t) ->
    Spec.t -> Global.t -> Global.t * Table.t * Table.t
  val perform : Spec.t -> Global.t -> Global.t * Table.t * Table.t
end

module MakeWithAccess (Sem:AccessSem.S) =
struct
  module Dom = Sem.Dom
  module AccessAnalysis = AccessAnalysis.Make (Sem)
  module Access = AccessAnalysis.Access
  module DUGraph = Dug.Make (Dom)
  module SsaDug = SsaDug.Make (DUGraph) (Access)
  module Worklist = Worklist.Make (DUGraph)
  module Table = MapDom.MakeCPO (Node) (Sem.Dom)
  module Spec = Sem.Spec
  module PowLoc = Sem.Dom.PowA
  type analysis_state = Worklist.t * Global.t * Table.t * Table.t

  let needwidening : bool -> DUGraph.node -> Worklist.t -> bool
  =fun should_widen idx wl -> should_widen && Worklist.is_loopheader idx wl

  (* FIFO-bounded def-locs cache. The def-locs of a node (union of its outgoing
     edge labels) are immutable, so caching them avoids recomputing per visit.
     But caching ALL nodes materialises ~the whole def-use relation as OCaml sets
     -- on emacs (153k nodes) that added ~9GB and blew the memory budget, undoing
     BDD's compression. Bound the cache to the active working set: at capacity,
     evict the oldest entry (a miss just recomputes from the BDD). Cap is 0 =
     unbounded (default for small runs); set via SPARROW_DEFLOCS_CACHE_CAP. *)
  let def_locs_cache = Hashtbl.create 251
  let def_locs_order : Node.t Queue.t = Queue.create ()
  let def_locs_cap =
    try int_of_string (Sys.getenv "SPARROW_DEFLOCS_CACHE_CAP") with _ -> 50000
  let get_def_locs : Node.t -> DUGraph.t -> Access.PowLoc.t
  = fun idx dug ->
    try Hashtbl.find def_locs_cache idx with Not_found ->
    let def_locs =
      let union_locs succ = PowLoc.union (DUGraph.get_abslocs idx succ dug) in
      DUGraph.fold_succ union_locs dug idx PowLoc.empty
    in
    if def_locs_cap > 0 && Hashtbl.length def_locs_cache >= def_locs_cap then
      (try Hashtbl.remove def_locs_cache (Queue.pop def_locs_order)
       with Queue.Empty -> ());
    Hashtbl.replace def_locs_cache idx def_locs;
    Queue.push idx def_locs_order;
    def_locs

  let clear_cache () =
    Hashtbl.clear def_locs_cache;
    Queue.clear def_locs_order;
    SsaDug.clear_cache ()

  let join_pairs_on_edge mem_edge pairs input =
    let add_one (input, stable) (loc, value) =
      if not (mem_edge loc) then (input, stable)
      else
      if Dom.B.eq value Dom.B.bot then (input, stable)
      else
        let old_value = Dom.find loc input in
        if Dom.B.le value old_value then (input, stable)
        else
          let joined_value =
            if Dom.B.le old_value value then value
            else Dom.B.join old_value value
          in
          (Dom.add loc joined_value input, false)
    in
    List.fold_left add_one (input, true) pairs

  let reset_iter_stats () =
    Hashtbl.clear iter_stats;
    Hashtbl.clear unstable_stats

  let bump_stat tbl node delta =
    let old = try Hashtbl.find tbl node with Not_found -> 0 in
    Hashtbl.replace tbl node (old + delta)

  let top_stats tbl limit =
    Hashtbl.fold (fun node count acc -> (node, count) :: acc) tbl []
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> BatList.take limit

  let print_sparse_iter_stats () =
    if !Options.sparse_iter_stats > 0 then
    begin
      let show_entry (node, count) =
        Node.to_string node ^ ":" ^ string_of_int count
      in
      let visits = top_stats iter_stats 8 |> List.map show_entry |> String.concat ", " in
      let unstable = top_stats unstable_stats 8 |> List.map show_entry |> String.concat ", " in
      my_prerr_endline ("\n#sparse hot visits: " ^ visits);
      my_prerr_endline ("#sparse hot unstable-locs: " ^ unstable)
    end

  let print_iteration idx =
    total_iterations := !total_iterations + 1;
    if !Options.sparse_iter_stats > 0 then bump_stat iter_stats idx 1;
    if !total_iterations = 1 then (g_clock := Sys.time(); l_clock := Sys.time ())
    else if !total_iterations mod 10000 = 0
    then
    begin
      let g_time = Format.sprintf "%.2f" (Sys.time() -. !g_clock) in
      let l_time = Format.sprintf "%.2f" (Sys.time() -. !l_clock) in
      my_prerr_string ("\r#iters: " ^ string_of_int !total_iterations
                        ^ " took " ^ g_time
                        ^ "s  ("  ^ l_time ^ "s / last 10000 iters)");
      flush stderr;
      if !Options.sparse_iter_stats > 0
         && !total_iterations mod !Options.sparse_iter_stats = 0
      then print_sparse_iter_stats ();
      l_clock := Sys.time ();
    end

  let propagate dug idx (works,inputof,outputof) (unstables,new_output,global)=
    let (works, inputof) =
      let update_succ succ (works, inputof) =
        let old_input = Table.find succ inputof in
        let mem_edge =
          if !Options.bdd_dug then
            let locs_on_edge = DUGraph.get_duset idx succ dug in
            fun x -> DUGraph.mem_duset x locs_on_edge
          else
            let locs_on_edge = DUGraph.get_abslocs idx succ dug in
            fun x -> PowLoc.mem x locs_on_edge
        in
        let (new_input, stable) =
          join_pairs_on_edge mem_edge unstables old_input
        in
        if stable then (works, inputof)
        else (Worklist.push idx succ works, Table.add succ new_input inputof)
      in
      DUGraph.fold_succ update_succ dug idx (works, inputof)
    in
    (works, global, inputof, Table.add idx new_output outputof)

  let get_unstable dug idx should_widen works old_output (new_output, global) =
    (* Use the node's DEFINED locs (cached, bounded by outgoing-edge labels) for
       the instability check -- including in BDD mode. The old BDD shortcut
       [Dom.keys new_output] scans the WHOLE output memory, which grows as the
       fixpoint accumulates state, so per-iteration cost grew without bound (the
       giants' fixpoint never converged). get_def_locs is computed once per node
       and reused; the result is unchanged (the extra pass-through locs are
       filtered out on each edge by mem_edge anyway). *)
    let def_locs =
      Profiler.event "SparseAnalysis.widening_get_def_locs" (get_def_locs idx) dug
    in
    let is_unstb v1 v2 = not (Dom.B.le v2 v1) in
    let u = Profiler.event "SparseAnalysis.widening_unstable" (Dom.unstables old_output new_output is_unstb) def_locs in
    if u = [] then None
    else
      let op =
        if needwidening should_widen idx works then Dom.B.widen
        else (fun _ y -> y)
      in
      let _ = Profiler.start_event "SparseAnalysis.widening_new_output" in
      let u = List.map (fun (k, v1, v2) -> (k, op v1 v2)) u in
      let new_output = list_fold (fun (k, v) -> Dom.add k v) u old_output in
      let _ = Profiler.finish_event "SparseAnalysis.widening_new_output" in
      if !Options.sparse_iter_stats > 0 then bump_stat unstable_stats idx (List.length u);
      (* update unstable locations's values by widened values *)
      let u = List.map (fun (k, _) -> (k, Dom.find k new_output)) u in
      Some (u, new_output, global)

  let prdbg_input : Node.t -> (Dom.t * Global.t) -> (Dom.t * Global.t)
  = fun node (mem, global) ->
    prerr_endline (Node.to_string node);
    prerr_endline (IntraCfg.Cmd.to_string (InterCfg.cmdof global.icfg node));
    prerr_endline "== Input ==";
    prerr_endline (Dom.to_string mem);
    (mem, global)

  let prdbg_output : Dom.t -> (Dom.t * Global.t) -> (Dom.t * Global.t)
  = fun old_output (new_output, global) ->
    prerr_endline "== Old Output ==";
    prerr_endline (Dom.to_string old_output);
    prerr_endline "== New Output ==";
    prerr_endline (Dom.to_string new_output);
    (new_output, global)

  let direct_transfer_scope _node f = f ()

  let direct_analysis_scope _spec _dug _idx _state f = f ()

  (* fixpoint iterator specialized to the widening phase *)
  let analyze_node :
    (DUGraph.node -> (unit -> Dom.t * Global.t) -> Dom.t * Global.t) ->
    Spec.t -> DUGraph.t -> DUGraph.node
    -> bool -> (Worklist.t * Global.t * Table.t * Table.t)
    -> (Worklist.t * Global.t * Table.t * Table.t)
  = fun transfer_scope spec dug idx should_widen (works, global, inputof, outputof) ->
    print_iteration idx;
    let old_output = Table.find idx outputof in
    (Table.find idx inputof, global)
    |> opt !Options.debug (prdbg_input idx)
    |> (fun input ->
        transfer_scope idx (fun () ->
            Profiler.event "SparseAnalysis.run" (Sem.run Strong spec idx)
              input))
    |> opt !Options.debug (prdbg_output old_output)
    |> Profiler.event "SparseAnalysis.get_unstable" (get_unstable dug idx should_widen works old_output)
    &> Profiler.event "SparseAnalysis.propagating" (propagate dug idx (works,inputof,outputof))
    |> (function None -> (works, global, inputof, outputof) | Some x -> x)

  let analyze_node_with_analysis_scope analysis_scope transfer_scope spec dug idx should_widen
      state =
    analysis_scope spec dug idx state (fun () ->
        analyze_node transfer_scope spec dug idx should_widen state)


  (* fixpoint iterator that can be used in both widening and narrowing phases *)
  let analyze_node_with_otable (widen,order) : Spec.t -> DUGraph.t ->
    DUGraph.node
    -> bool
    -> (Worklist.t * Global.t * Table.t * Table.t)
    -> (Worklist.t * Global.t * Table.t * Table.t)
  =fun spec dug idx _should_widen (works, global, inputof, outputof) ->
    print_iteration idx;
    let pred = DUGraph.pred idx dug in
    let input = List.fold_left (fun m p ->
          let pmem = Table.find p outputof in
          let locs_on_edge = DUGraph.get_abslocs p idx dug in
          PowLoc.fold (fun l m ->
              let v1 = Dom.find l pmem in
              let v2 = Dom.find l m in
              Dom.add l (Dom.B.join v1 v2) m) locs_on_edge m
          ) Dom.bot pred in
    let inputof = Table.add idx input inputof in
    let old_output = Table.find idx outputof in
    let (new_output, global) = Sem.run Strong spec idx (input, global) in
    let widened = widen old_output new_output in
    if order widened old_output then (works, global, inputof, outputof)
    else
      let works = Worklist.push_set idx (BatSet.of_list (DUGraph.succ idx dug)) works in
      (works, global, inputof, Table.add idx new_output outputof)

  let rec iterate f : DUGraph.t -> (Worklist.t * Global.t * Table.t * Table.t)
     -> (Worklist.t * Global.t * Table.t * Table.t)
  =fun dug (works, global, inputof, outputof) ->
    match Worklist.pick works with
    | None -> (works, global, inputof, outputof)
    | Some (idx, should_widen, rest) ->
      (rest, global, inputof, outputof)
      |> f dug idx should_widen
      |> iterate f dug

  let widening :
    ?analysis_scope:
      (Spec.t -> DUGraph.t -> DUGraph.node -> analysis_state ->
       (unit -> analysis_state) -> analysis_state) ->
    ?transfer_scope:(DUGraph.node -> (unit -> Dom.t * Global.t) -> Dom.t * Global.t) ->
    ?seed_nodes:Node.t BatSet.t ->
    Spec.t -> DUGraph.t -> (Worklist.t * Global.t * Table.t * Table.t)
      -> (Worklist.t * Global.t * Table.t * Table.t)
  =fun ?(analysis_scope=direct_analysis_scope)
       ?(transfer_scope=direct_transfer_scope) ?seed_nodes spec dug
       (worklist, global, inputof, outputof) ->
    total_iterations := 0;
    reset_iter_stats ();
    let t0 = Sys.time () in
    (* [seed_nodes] lets a seeded/incremental run push only the boundary nodes
       (modular link combine).  Default = every dug node = the standard full run. *)
    let seed_nodes =
      match seed_nodes with Some s -> s | None -> DUGraph.nodesof dug
    in
    worklist
    |> Worklist.push_init seed_nodes
    |> (fun init_worklist ->
        iterate
          (analyze_node_with_analysis_scope analysis_scope transfer_scope spec)
          dug
          (init_worklist, global, inputof, outputof))
    |> (fun x ->
        widen_time := Sys.time () -. t0;
        last_widen_iters := !total_iterations;
        print_sparse_iter_stats ();
        my_prerr_endline ("\n#iteration in widening : " ^ string_of_int !total_iterations); x)

  let narrowing ?(initnodes=BatSet.empty) : Spec.t -> DUGraph.t -> (Worklist.t * Global.t * Table.t * Table.t)
      -> (Worklist.t * Global.t * Table.t * Table.t)
  =fun spec dug (worklist, global, inputof, outputof) ->
    total_iterations := 0;
    reset_iter_stats ();
    let t0 = Sys.time () in
    worklist
    |> Worklist.push_init (if (BatSet.is_empty initnodes) then DUGraph.nodesof dug else initnodes)
    |> (fun init_worklist -> iterate (analyze_node_with_otable (Dom.narrow, fun x y -> Dom.le y x) spec)
        dug (init_worklist, global, inputof, outputof))
    |> (fun x ->
        narrow_time := Sys.time () -. t0;
        last_narrow_iters := !total_iterations;
        my_prerr_endline ("#iteration in narrowing : " ^ string_of_int !total_iterations); x)

  let print_dug (access,global,dug) =
    if !Options.dug then
    begin
      `Assoc
        [ ("callgraph", CallGraph.to_json global.callgraph);
          ("cfgs", InterCfg.to_json global.icfg);
          ("dugraph", DUGraph.to_json dug);
(*          ("dugraph-inter", DUGraph.to_json_inter dug access);*)
        ]
      |> Yojson.Safe.pretty_to_channel stdout;
      exit 0
    end
    else
    begin
      prerr_memory_usage ();
      prerr_endline ("#Nodes in def-use graph : " ^ i2s (DUGraph.nb_node dug));
      prerr_endline ("#Edges in def-use graph : " ^ i2s (DUGraph.nb_edge dug));
      prerr_endline ("#Locs on def-use graph : " ^ i2s (DUGraph.nb_loc dug));
    end

  let bind_fi_locs global mem_fi dug access inputof =
    DUGraph.fold_node (fun n t ->
      let used = Access.Info.useof (Access.find_node n access) in
      let pred = DUGraph.pred n dug in
      let locs_on_edge = list_fold (fun p -> PowLoc.union (DUGraph.get_abslocs p n dug)) pred PowLoc.empty in
      let locs_not_on_edge = PowLoc.diff used locs_on_edge in
      let mem_with_locs_not_on_edge =
        PowLoc.fold (fun loc mem ->
          Dom.add loc (Dom.find loc mem_fi)  mem
        ) locs_not_on_edge (Table.find n inputof) in
      Table.add n mem_with_locs_not_on_edge t
    ) dug inputof

  (* add pre-analysis memory to unanalyzed nodes *)
  let bind_unanalyzed_node global mem_fi dug access inputof =
    let nodes = InterCfg.nodesof global.icfg in
    let nodes_in_dug = DUGraph.nodesof dug in
    list_fold (fun node t ->
      if BatSet.mem node nodes_in_dug then t
      else
        let mem_with_access =
          PowLoc.fold (fun loc ->
            Dom.add loc (Dom.find loc mem_fi)
          ) (Access.Info.useof (Access.find_node node access)) Dom.bot in
          Table.add node mem_with_access t
    ) nodes inputof

  let initialize : Spec.t -> Global.t -> DUGraph.t -> Access.t -> Table.t
  = fun spec global dug access ->
    Table.add InterCfg.start_node (Sem.initial spec.Spec.locset) Table.empty
    |> cond (!Options.pfs < 100) (bind_fi_locs global spec.Spec.premem dug access) id

  let finalize spec global dug access (worklist, global, inputof, outputof) =
    let inputof =
      if !Options.pfs < 100 then bind_unanalyzed_node global spec.Spec.premem dug access inputof
      else inputof
    in
    (worklist, global, inputof, outputof)

  (* [seed_closed] (modular link COMBINE) PULL-initialises inputof from a seed
     outputof that carries ONLY the closed (boundary-independent) nodes' per-module
     values -- boundary nodes are absent (=> bot).  Mirrors the narrowing input
     computation: input[n] = join over predecessors p of outputof[p] on the p->n
     edge locs; closed predecessors contribute their seeded value, boundary
     predecessors contribute bot (recomputed during the boundary-only iteration).
     Then add the flow-insensitive locs (as [initialize] does) so a boundary node
     reading an fi-loc not on any edge still sees it.  This is the only sound way
     to seed under the PUSH-based widening loop: boundary nodes must start from bot
     with just the closed-predecessor contributions, never from a per-module value
     that has top-collapsed the (now-resolvable) import. *)
  let pull_seed_inputof spec global dug access seed_outputof =
    DUGraph.fold_node (fun n acc ->
      let input =
        List.fold_left (fun m p ->
          let pmem = Table.find p seed_outputof in
          let locs_on_edge = DUGraph.get_abslocs p n dug in
          PowLoc.fold (fun l m ->
            Dom.add l (Dom.B.join (Dom.find l pmem) (Dom.find l m)) m)
            locs_on_edge m)
          Dom.bot (DUGraph.pred n dug)
      in
      Table.add n input acc
    ) dug Table.empty
    (* seed start_node's initial mem (as [initialize] does); a boundary that
       includes the global-init proc must recompute it from this, not from bot. *)
    |> Table.add InterCfg.start_node (Sem.initial spec.Spec.locset)
    |> cond (!Options.pfs < 100)
         (bind_fi_locs global spec.Spec.premem dug access) id

  let print_spec : Spec.t -> unit
  = fun spec ->
    my_prerr_endline ("#total abstract locations  = " ^ string_of_int (PowLoc.cardinal spec.Spec.locset));
    my_prerr_endline ("#flow-sensitive abstract locations  = " ^ string_of_int (PowLoc.cardinal spec.Spec.locset_fs))

  let perform_with_scopes ?seed_closed transfer_scope analysis_scope spec global =
    print_spec spec;
    let access = StepManager.stepf false "Access Analysis" (AccessAnalysis.perform global spec.Spec.locset (Sem.run Strong spec)) spec.Spec.premem in
    let dug = StepManager.stepf false "Def-use graph construction" SsaDug.make (global, access, spec.Spec.locset_fs) in
    print_dug (access,global,dug);
    let file_of_opt =
      match !Options.worklist_order with
      | "file" ->
        Some (fun n ->
          try (InterCfg.cmdof global.icfg n |> IntraCfg.Cmd.location_of).Sparrow_cil.file
          with _ -> "")
      | _ -> None
    in
    let worklist =
      StepManager.stepf false "Workorder computation"
        (fun dug -> Worklist.init ?file_of:file_of_opt dug) dug
    in
    (* [seed_closed] (modular link COMBINE) supplies ONLY the closed nodes' outputs
       + the boundary; the engine PULL-initialises inputof from it over the DUG and
       iterates only the boundary (the sound seed for a cross-module boundary).
       Omitted = the standard full run. *)
    let default_widening_seed dug =
      match !Options.sparse_seed with
      | "all" -> None
      | "sources" ->
        Some (DUGraph.fold_node
                (fun n seeds ->
                   if DUGraph.pred n dug = [] then BatSet.add n seeds
                   else seeds)
                dug BatSet.empty)
      | mode -> failwith ("unknown -sparse_seed mode: " ^ mode)
    in
    let (init_inputof, init_outputof, widening_seed) =
      match seed_closed with
      | Some (seed_outputof, boundary) ->
        (* a seeded boundary node absent from THIS dug has no def-use
           influence here (nothing pulls from it, and its module value
           is already in the seed output table); it also has no work
           order, so queueing it would raise -- drop it from the seed *)
        let dug_nodes = DUGraph.nodesof dug in
        let boundary =
          BatSet.filter (fun n -> BatSet.mem n dug_nodes) boundary
        in
        (pull_seed_inputof spec global dug access seed_outputof,
         seed_outputof, Some boundary)
      | None -> (initialize spec global dug access, Table.empty, default_widening_seed dug)
    in
    (worklist, global, init_inputof, init_outputof)
    |> StepManager.stepf false "Fixpoint iteration with widening"
      (widening ~analysis_scope ~transfer_scope ?seed_nodes:widening_seed spec dug)
    |> finalize spec global dug access
    |> StepManager.stepf_opt !Options.narrow false "Fixpoint iteration with narrowing" (narrowing spec dug)
    |> (fun (_,global,inputof,outputof) -> (global, inputof, outputof))

  let perform_with_transfer_scope transfer_scope spec global =
    perform_with_scopes transfer_scope direct_analysis_scope spec global

  let perform : Spec.t -> Global.t -> Global.t * Table.t * Table.t
  =fun spec global ->
    perform_with_transfer_scope direct_transfer_scope spec global
end

module Make (Sem:AbsSem.S) = MakeWithAccess (AccessSem.Make (Sem))
