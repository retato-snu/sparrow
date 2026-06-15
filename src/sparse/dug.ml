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
open Yojson.Safe
open Global
open IntraCfg
open InterCfg
open BasicDom

module type S =
sig
  type t
  type node = BasicDom.Node.t
  module Loc : AbsDom.SET
  module PowLoc : PowDom.CPO with type elt = Loc.t
  module DUSet : sig type t end


  val create            : ?size : int -> ?loc_size : int -> unit -> t
  val nb_node           : t -> int
  val nb_edge           : t -> int
  val nb_loc            : t -> int
  val is_bdd            : t -> bool
  val nodesof           : t -> node BatSet.t

  val succ              : node -> t -> node list
  val pred              : node -> t -> node list

  val add_node          : node -> t -> t
  val add_edge          : node -> node -> t -> t
  val remove_node       : node -> t -> t
  val get_abslocs       : node -> node -> t -> PowLoc.t
  val get_duset         : node -> node -> t -> DUSet.t
  val mem_duset         : Loc.t -> DUSet.t -> bool
  val add_absloc        : node -> Loc.t -> node -> t -> t
  val add_abslocs       : node -> PowLoc.t -> node -> t -> t
  val remove_absloc     : node -> Loc.t -> node -> t -> t
  val remove_abslocs    : node -> PowLoc.t -> node -> t -> t
  val compact           : t -> t

(** {2 Iterator } *)

  val fold_node         : (node -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_edges        : (node -> node -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges        : (node -> node -> unit) -> t -> unit
  val fold_succ         : (node -> 'a ->'a) -> t -> node -> 'a -> 'a

(** {2 Print } *)

  val to_dot            : t -> string
  val to_json           : t -> Yojson.Safe.t
end

module MakeSet (Dom : InstrumentedMem.S) =
struct
  type node = BasicDom.Node.t
  module PowLoc = Dom.PowA
  module Loc = Dom.A
  module DUSet = struct type t = PowLoc.t end
  type loc = Loc.t
  type locset = PowLoc.t
  module G =
  struct
    module I = Graph.Imperative.Digraph.ConcreteBidirectional (BasicDom.Node)
    type t = { graph : I.t; label : ((node * node), locset) Hashtbl.t }
    let create ~size () = { graph = I.create ~size (); label = Hashtbl.create (2 * size) }

    let succ g n = I.succ g.graph n
    let pred g n = I.pred g.graph n
    let nb_vertex g = I.nb_vertex g.graph
    let pred_e g n = I.pred g.graph n |> List.map (fun p -> (p, Hashtbl.find g.label (p,n), n))
    let fold_vertex f g a = I.fold_vertex f g.graph a
    let fold_edges f g a = I.fold_edges f g.graph a
    let iter_edges f g = I.iter_edges f g.graph
    let fold_succ f g a = I.fold_succ f g.graph a

    let add_vertex g n = I.add_vertex g.graph n; g
    let remove_vertex g n = I.remove_vertex g.graph n; g
    let add_edge g s d = I.add_edge g.graph s d; g
    let add_edge_e g (s,locs,d) =
      Hashtbl.replace g.label (s,d) locs;
      add_edge g s d
    let remove_edge g s d = I.remove_edge g.graph s d; Hashtbl.remove g.label (s,d); g
    let find_label g s d = Hashtbl.find g.label (s,d)
    let modify_edge_def def g s d f =
      try
        let old_label = find_label g s d in
        let new_label = f old_label in
        Hashtbl.replace g.label (s,d) new_label;
        g
      with _ ->
        add_edge_e g (s, def, d)
  end

  type t = G.t

  let create ?(size=0) ?(loc_size=0) () =
    ignore loc_size;
    G.create ~size ()
  let is_bdd _ = false
  let nodesof dug = G.fold_vertex BatSet.add dug BatSet.empty

  let succ n dug = try G.succ dug n with _ -> []
  let pred n dug = try G.pred dug n with _ -> []
  let nb_node dug = G.nb_vertex dug
  let nb_edge dug = G.fold_edges (fun _ _ n -> n + 1) dug 0

  let remove_node : node -> t -> t
  =fun n dug -> G.remove_vertex dug n

  let add_node : node -> t -> t
  =fun n dug -> G.add_vertex dug n

  let add_edge : node -> node -> t -> t
  =fun src dst dug -> G.add_edge dug src dst

  let remove_edge : node -> node -> t -> t
  =fun src dst dug -> try G.remove_edge dug src dst with _ -> dug

  let get_abslocs : node -> node -> t -> locset
  =fun src dst dug -> try G.find_label dug src dst with _ -> PowLoc.empty

  let get_duset = get_abslocs

  let mem_duset : loc -> DUSet.t -> bool
  =fun x duset -> PowLoc.mem x duset

  let add_edge_e dug e = G.add_edge_e dug e

  let add_absloc : node -> Loc.t -> node -> t -> t
  =fun src x dst dug ->
    G.modify_edge_def (PowLoc.singleton x) dug src dst (PowLoc.add x)

  let add_abslocs : node -> locset -> node -> t -> t
  =fun src xs dst dug ->
    if PowLoc.is_empty xs then dug else
    G.modify_edge_def xs dug src dst (PowLoc.union xs)

  let replace_or_remove_abslocs src dst locs dug =
    if PowLoc.is_empty locs then remove_edge src dst dug
    else begin
      Hashtbl.replace dug.G.label (src, dst) locs;
      dug
    end

  let remove_absloc : node -> Loc.t -> node -> t -> t
  =fun src x dst dug ->
    try
      let locs = PowLoc.remove x (G.find_label dug src dst) in
      replace_or_remove_abslocs src dst locs dug
    with _ -> dug

  let remove_abslocs : node -> locset -> node -> t -> t
  =fun src xs dst dug ->
    if PowLoc.is_empty xs then dug else
    try
      let locs = PowLoc.diff (G.find_label dug src dst) xs in
      replace_or_remove_abslocs src dst locs dug
    with _ -> dug

  let compact dug = dug

  let fold_node = G.fold_vertex
  let fold_edges = G.fold_edges
  let iter_edges = G.iter_edges
  let fold_succ = G.fold_succ

  let nb_loc dug =
    fold_edges (fun src dst size ->
      PowLoc.cardinal (get_abslocs src dst dug) + size
    ) dug 0

  let succ_e : node -> t -> (node * locset) list
  =fun n g -> List.map (fun s -> (s, get_abslocs n s g)) (succ n g)

  let pred_e : node -> t -> (node * locset) list
  =fun n g -> List.map (fun p -> (p, get_abslocs p n g)) (pred n g)

  let to_dot : t -> string
  =fun dug ->
    "digraph dugraph {\n" ^
    fold_edges (fun src dst str ->
      let addrset = get_abslocs src dst dug in
        str ^ "\"" ^ (BasicDom.Node.to_string src) ^ "\"" ^ " -> " ^
              "\"" ^ (BasicDom.Node.to_string dst) ^ "\"" ^
              "[label=\"{" ^
                PowLoc.fold (fun addr s -> (Loc.to_string addr)^","^s) addrset "" ^
              "}\"]" ^ ";\n"
    ) dug ""
    ^ "}"

  let to_json : t -> Yojson.Safe.t
  = fun g ->
    let nodes = `List (fold_node (fun v nodes ->
                  (`String (BasicDom.Node.to_string v))::nodes) g [])
    in
    let edges = `List (fold_edges (fun src dst edges ->
                  let addrset = get_abslocs src dst g in
                  (`List [`String (BasicDom.Node.to_string src); `String (BasicDom.Node.to_string dst);
                          `String (PowLoc.fold (fun addr s -> (Loc.to_string addr)^","^s) addrset "")])
                  ::edges) g [])
    in
    `Assoc [("nodes", nodes); ("edges", edges)]
end

module MakeBDD (Dom : InstrumentedMem.S) =
struct
  type node = BasicDom.Node.t
  module PowLoc = Dom.PowA
  module Loc = Dom.A
  type loc = Loc.t
  type locset = PowLoc.t

  module I = Graph.Imperative.Digraph.ConcreteBidirectional (BasicDom.Node)
  module NodeTbl = Hashtbl.Make(struct
    type t = node
    let equal = BasicDom.Node.equal
    let hash = BasicDom.Node.hash
  end)
  module LocTbl = Hashtbl.Make(struct
    type t = loc
    let equal x y = Loc.compare x y = 0
    let hash = Hashtbl.hash
  end)

  module DUSet = struct
    type t = Current of locset * (loc -> int option) option
  end

		  type t = {
		    graph : I.t;
		    set_labels : ((node * node), locset) Hashtbl.t;
		    pending_labels : ((node * node), locset) Hashtbl.t;
		    node_ids : int NodeTbl.t;
		    loc_ids : int LocTbl.t;
	    locs_by_id : (int, loc) Hashtbl.t;
	    mutable next_node : int;
	    mutable next_loc : int;
	    mutable bdd_initialized : bool;
	    mutable nb_locs : int;
	    mutable pending_locs_approx : int;
	    max_nodes : int;
	    max_locs : int;
	  }

  let max_bdd_vars = 64

  let bits_for_count n =
    let rec loop bits cap =
      if cap >= max 1 n then bits else loop (bits + 1) (cap lsl 1)
    in
    loop 1 2

  let capacity_for_bits bits = 1 lsl bits

	  let create ?(size=0) ?(loc_size=0) () =
	    let node_bits = bits_for_count (max 2 size) in
	    let loc_bits =
	      if loc_size > 0 then bits_for_count (max 2 loc_size)
	      else max_bdd_vars - (2 * node_bits)
	    in
	    let total_bits = (2 * node_bits) + loc_bits in
	    if loc_bits < 1 then
	      failwith
	        ("BDD DUG cannot encode " ^ string_of_int size ^
	         " nodes with " ^ string_of_int max_bdd_vars ^ " BDD variables");
	    if total_bits > max_bdd_vars then
	      failwith
	        ("BDD DUG cannot encode " ^ string_of_int size ^ " nodes and " ^
	         string_of_int loc_size ^ " locations with " ^
	         string_of_int max_bdd_vars ^ " BDD variables");
		      { graph = I.create ~size ();
		        set_labels = Hashtbl.create (2 * size);
		        pending_labels = Hashtbl.create (2 * size);
		        node_ids = NodeTbl.create size;
	        loc_ids = LocTbl.create 251;
	        locs_by_id = Hashtbl.create 251;
	        next_node = 0;
	        next_loc = 0;
	        bdd_initialized = false;
	        nb_locs = 0;
	        pending_locs_approx = 0;
	        max_nodes = capacity_for_bits node_bits;
	        max_locs = capacity_for_bits loc_bits; }

  let ensure_bdd_initialized dug =
    if not dug.bdd_initialized then begin
      Bddset.init (dug.max_nodes - 1) (dug.max_nodes - 1) (dug.max_locs - 1);
      dug.bdd_initialized <- true
    end

  let ensure_node_id n dug =
    try NodeTbl.find dug.node_ids n with Not_found ->
      if dug.next_node >= dug.max_nodes then
        failwith "BDD DUG node-id capacity exhausted";
      let id = dug.next_node in
      dug.next_node <- dug.next_node + 1;
      NodeTbl.add dug.node_ids n id;
      id

  let find_node_id n dug =
    try Some (NodeTbl.find dug.node_ids n) with Not_found -> None

  let ensure_loc_id loc dug =
    try LocTbl.find dug.loc_ids loc with Not_found ->
      if dug.next_loc >= dug.max_locs then
        failwith "BDD DUG location-id capacity exhausted";
      let id = dug.next_loc in
      dug.next_loc <- dug.next_loc + 1;
      LocTbl.add dug.loc_ids loc id;
      Hashtbl.replace dug.locs_by_id id loc;
      id

  let find_loc_id loc dug =
    try Some (LocTbl.find dug.loc_ids loc) with Not_found -> None

	  let find_label labels src dst =
	    try Hashtbl.find labels (src, dst) with Not_found -> PowLoc.empty

	  let set_label src dst dug = find_label dug.set_labels src dst
	  let pending_label src dst dug = find_label dug.pending_labels src dst

	  let current_set_label src dst dug =
	    PowLoc.union (set_label src dst dug) (pending_label src dst dug)

  let is_bdd _ = true
  let nodesof dug = I.fold_vertex BatSet.add dug.graph BatSet.empty
  let succ n dug = try I.succ dug.graph n with _ -> []
  let pred n dug = try I.pred dug.graph n with _ -> []
  let nb_node dug = I.nb_vertex dug.graph
  let nb_edge dug = I.fold_edges (fun _ _ n -> n + 1) dug.graph 0

  let add_node n dug =
    ignore (ensure_node_id n dug);
    I.add_vertex dug.graph n;
    dug

  let add_edge src dst dug =
    ignore (ensure_node_id src dug);
    ignore (ensure_node_id dst dug);
    I.add_edge dug.graph src dst;
    dug

  let remove_node n dug =
    I.remove_vertex dug.graph n;
    dug

	  let get_abslocs src dst dug =
	    let set_locs = current_set_label src dst dug in
	    if not dug.bdd_initialized then set_locs
    else match find_node_id src dug, find_node_id dst dug with
    | Some src_id, Some dst_id when Bddset.subset_sd src_id dst_id ->
      let rec fold acc =
        match Bddset.next () with
        | -1 -> acc
        | loc_id ->
          let loc = Hashtbl.find dug.locs_by_id loc_id in
          fold (PowLoc.add loc acc)
      in
      fold set_locs
    | _ -> set_locs

  let get_duset src dst dug =
    (* Eager materialization (soundness fix). The previous lazy BDD-membership
       path relied on a GLOBAL mutable sub_bdd: subset_sd (here) set it, mem_sub
       (in mem_duset) queried it. sparseAnalysis defers the mem_duset closure
       past other edges' get_duset calls, which clobber the global sub_bdd ->
       wrong def-use membership -> unsound divergence (less-382: 331/606 vs the
       correct 325/612). Materializing the edge's locset now makes mem_duset a
       pure, reentrant set test. Storage stays BDD-compressed; only this edge's
       transient locset is built -- exactly what get_abslocs already does on the
       hot path, so no new asymptotic cost. *)
    DUSet.Current (get_abslocs src dst dug, None)

  let mem_duset loc duset =
    match duset with
    | DUSet.Current (set_locs, bdd) ->
      PowLoc.mem loc set_locs ||
      match bdd with
      | None -> false
      | Some find_id ->
        match find_id loc with
        | None -> false
        | Some loc_id -> Bddset.mem_sub loc_id

	  let add_abslocs_to_bdd src locs dst dug =
	    ensure_bdd_initialized dug;
	    let src_id = ensure_node_id src dug in
	    let dst_id = ensure_node_id dst dug in
	    let loc_ids =
	      PowLoc.fold
	        (fun loc ids -> ensure_loc_id loc dug :: ids)
	        locs []
	    in
	    begin match loc_ids with
	    | [] -> ()
	    | [loc_id] -> Bddset.add (src_id, dst_id, loc_id)
	    | _ -> Bddset.add_set (src_id, dst_id, Array.of_list loc_ids)
	    end;
	    dug.nb_locs <- dug.nb_locs + List.length loc_ids;
		    I.add_edge dug.graph src dst;
		    dug

		  let add_abslocs_to_set src locs dst dug =
		    if PowLoc.is_empty locs then dug
		    else begin
		      let set_locs = set_label src dst dug in
		      let set_locs' = PowLoc.union locs set_locs in
		      if set_locs' != set_locs then
		        Hashtbl.replace dug.set_labels (src, dst) set_locs';
		      I.add_edge dug.graph src dst;
		      dug
		    end

		  let buffer_abslocs src locs dst dug =
		    if PowLoc.is_empty locs then dug
		    else begin
		      let set_locs = pending_label src dst dug in
		      let set_locs' = PowLoc.union locs set_locs in
		      if set_locs' != set_locs then begin
		        Hashtbl.replace dug.pending_labels (src, dst) set_locs';
		        dug.pending_locs_approx <-
		          dug.pending_locs_approx + PowLoc.cardinal locs
	      end;
	      I.add_edge dug.graph src dst;
	      dug
	    end

	  let add_absloc src loc dst dug =
	    if !Options.bdd_compact then
	      buffer_abslocs src (PowLoc.singleton loc) dst dug
	    else
	      (* Store the loc directly in the BDD (as the khheo reference does), NOT
	         in an OCaml set_labels map. ssaDug builds the bulk of the DUG via
	         per-loc add_absloc, so the old set_labels path kept nearly all
	         intra-procedural edge labels as PowLoc sets -- defeating BDD's whole
	         memory purpose: emacs construction climbed past 16GB (vs the paper's
	         7.8GB) and was capped. Routing single-loc adds into the BDD keeps the
	         def-use relation compressed; set_labels stays empty in non-compact. *)
	      add_abslocs_to_bdd src (PowLoc.singleton loc) dst dug

	  let add_abslocs src locs dst dug =
	    if PowLoc.is_empty locs then dug
	    else if !Options.bdd_compact then buffer_abslocs src locs dst dug
	    else add_abslocs_to_bdd src locs dst dug

	  let remove_absloc _src _loc _dst dug = dug
	  let remove_abslocs _src _locs _dst dug = dug

		  let compact dug =
		    if not !Options.bdd_compact then dug
		    else
		      let labels =
		        Hashtbl.fold
		          (fun (src, dst) locs labels -> (src, locs, dst) :: labels)
		          dug.pending_labels []
		      in
		      Hashtbl.clear dug.pending_labels;
		      List.fold_left
		        (fun dug (src, locs, dst) ->
		           if PowLoc.cardinal locs <= !Options.bdd_compact_set_threshold then
		             add_abslocs_to_set src locs dst dug
		           else
		             add_abslocs_to_bdd src locs dst dug)
		        dug labels

		  let fold_node f dug a = I.fold_vertex f dug.graph a
  let fold_edges f dug a = I.fold_edges f dug.graph a
  let iter_edges f dug = I.iter_edges f dug.graph
  let fold_succ f dug n a = I.fold_succ f dug.graph n a

		  let nb_loc dug =
		    let count labels n =
		      Hashtbl.fold
		        (fun _ locs n -> n + PowLoc.cardinal locs)
		        labels n
		    in
		    dug.nb_locs
		    |> count dug.set_labels
		    |> count dug.pending_labels

  let to_dot dug =
    "digraph dugraph {\n" ^
    fold_edges (fun src dst str ->
      let addrset = get_abslocs src dst dug in
      str ^ "\"" ^ (BasicDom.Node.to_string src) ^ "\"" ^ " -> " ^
      "\"" ^ (BasicDom.Node.to_string dst) ^ "\"" ^
      "[label=\"{" ^
      PowLoc.fold (fun addr s -> (Loc.to_string addr)^","^s) addrset "" ^
      "}\"]" ^ ";\n"
    ) dug ""
    ^ "}"

  let to_json dug =
    let nodes =
      `List (fold_node (fun v nodes ->
        (`String (BasicDom.Node.to_string v))::nodes) dug [])
    in
    let edges =
      `List (fold_edges (fun src dst edges ->
        let addrset = get_abslocs src dst dug in
        (`List [`String (BasicDom.Node.to_string src);
                `String (BasicDom.Node.to_string dst);
                `String (PowLoc.fold (fun addr s -> (Loc.to_string addr)^","^s) addrset "")])
        ::edges) dug [])
    in
    `Assoc [("nodes", nodes); ("edges", edges)]
end

module Make (Dom : InstrumentedMem.S) =
struct
  module Set = MakeSet(Dom)
  module BDD = MakeBDD(Dom)
  module Loc = Dom.A
  module PowLoc = Dom.PowA
  module DUSet = struct
    type t = SetDU of Set.DUSet.t | BDDDU of BDD.DUSet.t
  end

  type node = BasicDom.Node.t
  type t = Set of Set.t | BDD of BDD.t

	  (* Size-gated representation choice. The BDD DUG is the memory-at-scale
	     representation: it is slower and heavier than the Set DUG on programs
	     that fit in memory, and only pays off past the memory wall (~150k DUG
	     nodes, the emacs scale, where the Set DUG OOMs). So pick Set by default
	     and switch to BDD only when forced (-bdd_dug) or when -bdd_auto fires at
	     the node-count threshold. [size] is the DUG node count from SsaDug. *)
	  let create ?(size=0) ?(loc_size=0) () =
	    let use_bdd =
	      !Options.bdd_dug
	      || (!Options.bdd_auto && size >= !Options.bdd_auto_threshold)
	    in
	    if use_bdd then BDD (BDD.create ~size ~loc_size ())
	    else Set (Set.create ~size ~loc_size ())

  let nb_node = function Set g -> Set.nb_node g | BDD g -> BDD.nb_node g
  let nb_edge = function Set g -> Set.nb_edge g | BDD g -> BDD.nb_edge g
  let nb_loc = function Set g -> Set.nb_loc g | BDD g -> BDD.nb_loc g
  let is_bdd = function Set g -> Set.is_bdd g | BDD g -> BDD.is_bdd g
  let nodesof = function Set g -> Set.nodesof g | BDD g -> BDD.nodesof g
  let succ n = function Set g -> Set.succ n g | BDD g -> BDD.succ n g
  let pred n = function Set g -> Set.pred n g | BDD g -> BDD.pred n g
  let add_node n = function Set g -> Set (Set.add_node n g) | BDD g -> BDD (BDD.add_node n g)
  let add_edge src dst = function Set g -> Set (Set.add_edge src dst g) | BDD g -> BDD (BDD.add_edge src dst g)
  let remove_node n = function Set g -> Set (Set.remove_node n g) | BDD g -> BDD (BDD.remove_node n g)
  let get_abslocs src dst = function Set g -> Set.get_abslocs src dst g | BDD g -> BDD.get_abslocs src dst g
  let get_duset src dst = function
    | Set g -> DUSet.SetDU (Set.get_duset src dst g)
    | BDD g -> DUSet.BDDDU (BDD.get_duset src dst g)
  let mem_duset loc = function
    | DUSet.SetDU duset -> Set.mem_duset loc duset
    | DUSet.BDDDU duset -> BDD.mem_duset loc duset
	  let add_absloc src loc dst = function Set g -> Set (Set.add_absloc src loc dst g) | BDD g -> BDD (BDD.add_absloc src loc dst g)
	  let add_abslocs src locs dst = function Set g -> Set (Set.add_abslocs src locs dst g) | BDD g -> BDD (BDD.add_abslocs src locs dst g)
	  let remove_absloc src loc dst = function Set g -> Set (Set.remove_absloc src loc dst g) | BDD g -> BDD (BDD.remove_absloc src loc dst g)
	  let remove_abslocs src locs dst = function Set g -> Set (Set.remove_abslocs src locs dst g) | BDD g -> BDD (BDD.remove_abslocs src locs dst g)
	  let compact = function Set g -> Set (Set.compact g) | BDD g -> BDD (BDD.compact g)
	  let fold_node f g a = match g with Set g -> Set.fold_node f g a | BDD g -> BDD.fold_node f g a
  let fold_edges f g a = match g with Set g -> Set.fold_edges f g a | BDD g -> BDD.fold_edges f g a
  let iter_edges f = function Set g -> Set.iter_edges f g | BDD g -> BDD.iter_edges f g
  let fold_succ f g n a = match g with Set g -> Set.fold_succ f g n a | BDD g -> BDD.fold_succ f g n a
  let to_dot = function Set g -> Set.to_dot g | BDD g -> BDD.to_dot g
  let to_json = function Set g -> Set.to_json g | BDD g -> BDD.to_json g
end
