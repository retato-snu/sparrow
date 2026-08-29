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
(** Map domain *)

module type CPO =
sig
  include AbsDom.CPO
  module A : AbsDom.SET
  module B : AbsDom.CPO
  module PowA : PowDom.CPO with type elt = A.t

  (* Reset this instance's value hash-cons table (b_table).  The table pins one
     copy of every distinct B.t ever hashconsed and is never cleared on its own;
     a single whole-program analysis stays bounded, but a client that runs MANY
     independent solves in one process (the modular per-module open-solve +
     re-solves) accumulates every solve's values and leaks until OOM.  Clearing
     between independent solves is sound -- hash-consing is only an optimization,
     all existing values remain valid -- it only drops cross-solve physical
     sharing.  No-op for single-solve clients (they never call it). *)
  val clear_b_table : unit -> unit

  (* Enable/disable value hash-consing for this instance.  Hash-consing dedups by
     a structural Hashtbl lookup; that lookup deep-compares values, which walks the
     CYCLIC Cil.typ carried inside Locs.  A whole-program analysis is unaffected
     (its equal values are physically shared, so compare short-circuits on ==), but
     across SEPARATE COMPILATION (Marshaled artifacts) sharing is broken and the
     deep compare overflows (spurious Out_of_memory).  The modular link disables it;
     the domain's own ops use the cycle-safe Loc.compare instead. *)
  val set_b_hashcons : bool -> unit

  val empty : t
  val is_empty : t -> bool
  val find : A.t -> t -> B.t
  (** Return the comparison-equivalent key actually resident in the map,
      together with its value.  Unlike [find], this preserves key
      representatives when a client builds a restricted map from keys
      decoded independently of the source map. *)
  val find_binding : A.t -> t -> (A.t * B.t) option
  (** Reinsert an already-resident binding without key or value
      normalization.  This is the key-driven counterpart of [filter]: it is
      intended only for rebuilding a restriction from [find_binding]
      results. *)
  val add_binding : A.t -> B.t -> t -> t
  val add : A.t -> B.t -> t -> t
  val weak_add : A.t -> B.t -> t -> t
  val remove : A.t -> t -> t
  val map : (B.t -> B.t) -> t -> t
  val mapi : (A.t -> B.t -> B.t) -> t -> t
  val fold : (A.t -> B.t -> 'a -> 'a) -> t -> 'a -> 'a
  val foldi : (A.t -> B.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (A.t -> B.t -> unit) -> t -> unit
  val mem : A.t -> t -> bool
  val filter : (A.t -> B.t -> bool) -> t -> t
  val cardinal : t -> int
  val choose : t -> (A.t * B.t)
  val to_string : t -> string
  val for_all : (A.t -> B.t -> bool) -> t -> bool
  val exists : (A.t -> B.t -> bool) -> t -> bool
  val keys : t -> PowA.t

  val unstables : t -> t -> (B.t -> B.t -> bool) -> PowA.t
    -> (A.t * B.t * B.t) list

  val join_pairs : (A.t * B.t) list -> t -> t
  val widen_pairs : (A.t * B.t) list -> t -> t
  val meet_big_small : t -> t -> t
end

module type LAT =
sig
  include AbsDom.LAT
  include CPO with type t := t
end

module MakeCPO (A:AbsDom.SET)(B:AbsDom.CPO) : CPO
  with type t = B.t BatMap.Make(A).t
  and type PowA.t = PowDom.MakeCPO(A).t
  and type A.t = A.t and type B.t = B.t

module MakeLAT (A:AbsDom.SET)(B:AbsDom.CPO) : LAT
  with type A.t = A.t and type B.t = B.t
