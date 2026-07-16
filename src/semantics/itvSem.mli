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

(** [(active, generation)] for the dynamically scoped transfer override.
    [generation] changes on both installation and restoration, allowing a
    staged transfer authorization to reject an intervening override even
    after its scope has ended. *)
val transfer_hook_status : unit -> bool * int

val eval_lv : ?spec:Spec.t -> BasicDom.Proc.t -> Sparrow_cil.lval -> ItvDom.Mem.t -> BasicDom.PowLoc.t
val eval : ?spec:Spec.t -> BasicDom.Proc.t -> Sparrow_cil.exp -> ItvDom.Mem.t -> ItvDom.Val.t
val eval_array_alloc : ?spec:Spec.t -> BasicDom.Node.t -> Sparrow_cil.exp -> bool -> Dom.t -> ItvDom.Val.t
val eval_string_alloc : BasicDom.Node.t -> string -> Dom.t -> ItvDom.Val.t
