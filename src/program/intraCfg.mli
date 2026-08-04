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
(** Intra-procedural CFG *)
module Node : sig
  include AbsDom.HASHABLE_SET
  val entry : t
  val exit : t
  val id : t -> int
  val get_initial_id : unit -> int
  val get_next_id : unit -> int
  val set_next_id : int -> unit
end

module NodeSet : BatSet.S with type elt = Node.t

module Cmd : sig
  type t =
  | Cinstr of Sparrow_cil.instr list
  | Cif of Sparrow_cil.exp * Sparrow_cil.block * Sparrow_cil.block * Sparrow_cil.location
  | CLoop of Sparrow_cil.location
  (* final graph has the following cmds only *)
  | Cset of Sparrow_cil.lval * Sparrow_cil.exp * Sparrow_cil.location
  | Cexternal of Sparrow_cil.lval * Sparrow_cil.location
  | Calloc of Sparrow_cil.lval * alloc * static * Sparrow_cil.location
  | Csalloc of Sparrow_cil.lval * string * Sparrow_cil.location
  | Cfalloc of Sparrow_cil.lval * Sparrow_cil.fundec * Sparrow_cil.location
  | Cassume of Sparrow_cil.exp * Sparrow_cil.location
  | Ccall of Sparrow_cil.lval option * Sparrow_cil.exp * Sparrow_cil.exp list * Sparrow_cil.location
  | Creturn of Sparrow_cil.exp option * Sparrow_cil.location
  | Casm of Sparrow_cil.attributes * string list *
            (string option * string * Sparrow_cil.lval) list *
            (string option * string * Sparrow_cil.exp) list *
            string list * Sparrow_cil.location
  | Cskip
  and alloc = Array of Sparrow_cil.exp | Struct of Sparrow_cil.compinfo
  and static = bool

  val fromCilStmt : Sparrow_cil.stmtkind -> t
  val to_string : t -> string
  val location_of : t -> Sparrow_cil.location
end

(** Abstract type of intra-procedural CFG *)
type t
and node = Node.t
and cmd = Cmd.t

type global_provenance_item_kind =
  | Global_variable_declaration
  | Global_variable_definition
  | Global_function_definition

type global_provenance_outcome =
  | Global_chain_nodes of Node.t list
  | Global_cfg_dropped

type global_provenance_row = {
  global_provenance_global_index : int;
  global_provenance_chain_index : int;
  global_provenance_item_index : int;
  global_provenance_initializer_index : int option;
  global_provenance_name : string;
  global_provenance_kind : global_provenance_item_kind;
  global_provenance_location : Sparrow_cil.location;
  global_provenance_pretrim_nodes : Node.t list;
  global_provenance_dropped_nodes : Node.t list;
  global_provenance_drop_mechanisms : string list;
  global_provenance_outcome : global_provenance_outcome;
}

(** Closed role vocabulary for a generated temporary in the synthetic [_G_]
    construction.  These roles describe the source construction event, not a
    later CFG opcode or an emitted-command ordinal. *)
type construction_temp_generation_role =
  | Aggregate_storage
  | Nested_array_storage
  | Field_storage
  | Array_loop_index
  | String_literal
  | Sizeof_string

type construction_temp_outcome =
  | Construction_temp_survives of Node.t list
  | Construction_temp_absorbed_by of Node.t list
  | Construction_temp_dropped

(** Observation-only creation record for a synthetic [_G_] temporary.
    [construction_temp_initializer_or_type_path] and
    [construction_temp_expression_child_path] are captured before the final
    temporary spelling and node ordinal can become identity.  Type and
    payload spellings are canonical preimages; consumers hash them under the
    versioned source-path token contract. *)
type construction_temp_provenance_row = {
  construction_temp_owner_global_index : int;
  construction_temp_owner_chain_index : int;
  construction_temp_owner_initializer_index : int option;
  construction_temp_owner_name : string;
  construction_temp_owner_kind : global_provenance_item_kind;
  construction_temp_owner_location : Sparrow_cil.location;
  construction_temp_initializer_or_type_path : string;
  construction_temp_expression_child_path : string;
  construction_temp_generation_role : construction_temp_generation_role;
  construction_temp_type_preimage : string;
  construction_temp_payload_preimage : string;
  construction_temp_final_name : string;
  construction_temp_pretrim_nodes : Node.t list;
  construction_temp_outcome : construction_temp_outcome;
}

val string_of_construction_temp_generation_role :
  construction_temp_generation_role -> string

(** A constructive identity witness for one allocation site in a completed
    per-procedure CFG.  The cross-build key is translation unit, original
    function, source location, syntactic role, and deterministic occurrence
    index.  Optional identity fields remain absent when the merged-build
    observer cannot establish them uniquely.  [allocation_provenance_site_id]
    is exactly the internal allocation-site identifier consumed by the
    abstract domains. *)
type allocation_site_provenance_row = {
  allocation_provenance_procedure : string;
  allocation_provenance_translation_unit : string option;
  allocation_provenance_original_function : string option;
  allocation_provenance_location : Sparrow_cil.location;
  allocation_provenance_role : string;
  allocation_provenance_occurrence_index : int;
  allocation_provenance_site_id : string;
  allocation_provenance_node : Node.t;
  allocation_provenance_is_string : bool;
}

(** Out-of-band observation switch used only by independence gates. *)
val allocation_site_provenance_recording : bool ref

(** Read the allocation-site table from a completed CFG.  This function only
    inspects existing commands; it never owns or mutates graph state. *)
val allocation_site_provenance_rows : t -> allocation_site_provenance_row list

(** Out-of-band observation switch used only by the independence gate. *)
val global_provenance_recording : bool ref

(** Rows from the most recent global-CFG construction, finalized at the
    unreachable-node trimming decision. *)
val global_provenance_rows : unit -> global_provenance_row list

(** Temp-event rows from the most recent global-CFG construction.  The table
    uses the same recording switch and follows graph replacement in parallel
    with {!global_provenance_rows}. *)
val construction_temp_provenance_rows :
  unit -> construction_temp_provenance_row list

(** Classify generated global-chain nodes immediately before the caller
    removes [unreachable]. *)
val finish_global_provenance : unreachable:NodeSet.t -> t -> unit

val init : Sparrow_cil.fundec -> Sparrow_cil.location -> t
val generate_module_global_proc :
  Sparrow_cil.global list -> Sparrow_cil.fundec -> t
val generate_global_proc : Sparrow_cil.global list -> Sparrow_cil.fundec -> t

val get_pid : t -> string
val get_fd : t -> Sparrow_cil.fundec
val copy_with_pid : string -> t -> t
val get_formals : t -> Sparrow_cil.varinfo list
val get_scc_list : t -> node list list

val nodesof : t -> node list
val entryof : t -> node
val exitof : t -> node
val callof : node -> t -> node
val returnof : node -> t -> node

val is_entry : node -> bool
val is_exit : node -> bool
val is_callnode : node -> t -> bool
val is_returnnode : node -> t -> bool
val is_inside_loop : node -> t -> bool

val find_cmd : node -> t ->  cmd

val unreachable_node : t -> NodeSet.t

val compute_scc : t -> t

val optimize : t -> t
val with_collision_safe_global_replacement : (unit -> 'a) -> 'a

val fold_node : (node -> 'a -> 'a) -> t -> 'a -> 'a
val fold_edges : (node -> node -> 'a -> 'a) -> t -> 'a -> 'a

(** {2 Predecessors and Successors } *)

val pred : node -> t -> node list
val succ : node -> t -> node list

(** {2 Graph Manipulation } *)

val add_cmd : node -> cmd -> t -> t
val add_new_node : node -> cmd -> node -> t -> t
val add_node_with_cmd : node -> cmd -> t -> t
val add_edge : node -> node -> t -> t
val remove_node : node -> t -> t

(** {2 Dominators } *)

val compute_dom : t -> t

(** [dom_fronts n g] returns dominance frontiers of node [n] in graph [g] *)
val dom_fronts : node -> t -> NodeSet.t
val children_of_dom_tree : node -> t -> NodeSet.t
val parent_of_dom_tree : node -> t -> node option

(** {2 Print } *)

val print_dot : out_channel -> t -> unit
val to_json : t -> Yojson.Safe.t
