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
(** Abstract semantics of interval analysis *)
include AbsSem.S with type Dom.t = ItvDom.Mem.t and type Dom.A.t = BasicDom.Loc.t and type Dom.PowA.t = BasicDom.PowLoc.t

val with_transfer_hook :
  (AbsSem.update_mode -> Spec.t -> BasicDom.Node.t -> Dom.t -> Global.t ->
   (Dom.t * Global.t) option) ->
  (unit -> 'a) -> 'a

val lookup : BasicDom.PowLoc.t -> ItvDom.Mem.t -> ItvDom.Val.t
val can_strong_update :
  AbsSem.update_mode -> Spec.t -> Global.t -> BasicDom.PowLoc.t -> bool
val eval_const : Sparrow_cil.constant -> ItvDom.Val.t
val eval_uop : Spec.t -> Sparrow_cil.unop -> ItvDom.Val.t -> ItvDom.Val.t
val eval_bop :
  Spec.t -> Sparrow_cil.binop -> ItvDom.Val.t -> ItvDom.Val.t -> ItvDom.Val.t

val eval_lv : ?spec:Spec.t -> BasicDom.Proc.t -> Sparrow_cil.lval -> ItvDom.Mem.t -> BasicDom.PowLoc.t
val eval : ?spec:Spec.t -> BasicDom.Proc.t -> Sparrow_cil.exp -> ItvDom.Mem.t -> ItvDom.Val.t
val eval_array_alloc : ?spec:Spec.t -> BasicDom.Node.t -> Sparrow_cil.exp -> bool -> Dom.t -> ItvDom.Val.t
val eval_struct_alloc : BasicDom.PowLoc.t -> Sparrow_cil.compinfo -> ItvDom.Val.t
val eval_string_alloc : BasicDom.Node.t -> string -> Dom.t -> ItvDom.Val.t
val eval_string : string -> ItvDom.Val.t
