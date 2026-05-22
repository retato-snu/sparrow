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
(** Worklist *)

module type S =
sig
  module DUGraph : Dug.S
  type t
  type order_entry = {
    node : BasicDom.Node.t;
    order : int;
    loop_header : bool;
    head_order : int option;
  }
  type snapshot = {
    order : order_entry list;
    sccs : BasicDom.Node.t list list;
    loop_headers : BasicDom.Node.t list;
  }
  val init : DUGraph.t -> t
  val pick : t -> (BasicDom.Node.t * t) option
  val push : BasicDom.Node.t -> BasicDom.Node.t -> t -> t
  val push_set : BasicDom.Node.t -> BasicDom.Node.t BatSet.t -> t -> t
  val is_loopheader : BasicDom.Node.t -> t -> bool
  val snapshot : t -> snapshot
end

module Make(DUGraph : Dug.S) : S with type DUGraph.t = DUGraph.t
