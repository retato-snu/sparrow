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

val last_pre_iters : int ref

(** The raw flow-insensitive fixpoint: one shared memory over the given
    node list, whole-memory widening once per round, starting at round
    number [k].  Exported for the modular compile driver (A3), which runs
    it module-alone from bottom under an import-silencing transfer hook --
    deliberately WITHOUT [perform]'s call-edge drawing / callgraph /
    unreachable-function removal, all of which are link-time products.
    Mli-only export; behavior unchanged. *)
val fixpt :
  InterCfg.Node.t list -> int -> ItvDom.Mem.t * Global.t ->
  ItvDom.Mem.t * Global.t

val perform : Global.t -> Global.t
