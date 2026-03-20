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
(** Alarm Sparrow_cil.expression *)
type t =
  | ArrayExp of Sparrow_cil.lval * Sparrow_cil.exp * Sparrow_cil.location
  | DerefExp of Sparrow_cil.exp * Sparrow_cil.location
  | DivExp of Sparrow_cil.exp * Sparrow_cil.exp * Sparrow_cil.location
  | Strcpy of Sparrow_cil.exp * Sparrow_cil.exp * Sparrow_cil.location
  | Strcat of Sparrow_cil.exp * Sparrow_cil.exp * Sparrow_cil.location
  | Strncpy of Sparrow_cil.exp * Sparrow_cil.exp * Sparrow_cil.exp * Sparrow_cil.location
  | Memcpy of Sparrow_cil.exp * Sparrow_cil.exp * Sparrow_cil.exp *  Sparrow_cil.location
  | Memmove of Sparrow_cil.exp * Sparrow_cil.exp * Sparrow_cil.exp * Sparrow_cil.location
  | AllocSize of Sparrow_cil.exp * Sparrow_cil.location

val collect : IntraCfg.cmd -> t list
val to_string : t -> string

