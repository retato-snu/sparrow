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
(** Inter-procedural CFG *)

module Proc : AbsDom.HASHABLE_SET with type t = string
module ProcSet : BatSet.S with type elt = Proc.t

module Node : sig
  include AbsDom.HASHABLE_SET
  val get_pid   : t -> Proc.t
  val get_cfgnode : t -> IntraCfg.Node.t
  val make      : Proc.t -> IntraCfg.Node.t -> t
end

module NodeSet : BatSet.S with type elt = Node.t

(** Abstract type of inter-procedural CFG *)
type t
and pid = Proc.t
and node = Node.t

val global_proc : Proc.t
(** Starting point of program *)
val start_node : node

val init : Sparrow_cil.file -> t

(** module (translation-unit) variant of [init]: the global proc only
    initializes globals and does not call main, so a main-less library
    module still yields a complete InterCfg *)
val init_module : Sparrow_cil.file -> t

(** stable (function-local) node-id variants: a function's node ids and
    allocation-site identities depend only on its own body, so a
    per-module solved memory can be seeded into a merged link *)
val init_stable : Sparrow_cil.file -> t
val init_module_stable : Sparrow_cil.file -> t

(** A4 link seam: assemble an InterCfg directly from already-built
    per-procedure CFGs (module objects' functions + the link-synthesized
    [_G_]).  [call_edges] start empty -- call resolution is the link
    pre-analysis's product.  Dominators/SCCs are not recomputed (the
    shipped per-function structures carry them). *)
val of_cfgs : (pid * IntraCfg.t) list -> Sparrow_cil.global list -> t

val cfgof : t -> pid -> IntraCfg.t
val argsof : t -> pid -> Sparrow_cil.varinfo list
val cmdof : t -> Node.t -> IntraCfg.cmd
val add_cmd : t -> Node.t -> IntraCfg.cmd -> t

(* Procedure-cloning primitives (call-site polyvariance). *)
val add_cfg : pid -> IntraCfg.t -> t -> t
val mem_cfg : pid -> t -> bool
val compute_dom_and_scc : t -> t
val insert_global_cfalloc :
  Sparrow_cil.varinfo -> Sparrow_cil.fundec -> t -> t

val pidsof : t -> pid list
val nodesof : t -> Node.t list
val entryof : t -> pid -> node
val exitof : t -> pid -> node
val callof : node -> t -> node
val returnof : node -> t -> node

val is_callnode : node -> t -> bool
val is_returnnode : node -> t -> bool
val is_inside_loop : node -> t -> bool

val callnodesof : t -> node list

val add_call_edge : Node.t -> Proc.t -> t -> t
val set_call_edge : Node.t -> ProcSet.t -> t -> t
val get_callees : Node.t -> t -> ProcSet.t
val is_undef : pid -> t -> bool


val remove_function : pid -> t -> t
val remove_node : node -> t -> t

val unreachable_node : t -> NodeSet.t


val fold_cfgs : (Proc.t -> IntraCfg.t -> 'a -> 'a) -> t -> 'a -> 'a


val nodes_of_pid : t -> pid -> Node.t list

(** {2 Print } *)

val to_json : t -> Yojson.Safe.t
