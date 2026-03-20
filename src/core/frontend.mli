(***********************************************************************)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(*                                                                     *)
(***********************************************************************)
(** Frontend *)
open Sparrow_cil

val files : string list ref
val marshal_file : string ref
val args : string -> unit
val parse : unit -> Sparrow_cil.file
val makeCFGinfo : Sparrow_cil.file -> Sparrow_cil.file
val is_varargs : string -> Sparrow_cil.file -> bool
val inline : Global.t -> bool
