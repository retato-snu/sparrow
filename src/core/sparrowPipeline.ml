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
open Global

(* transformation based on syntactic heuristics *)
let transform_simple file =
  Vocab.opt !Options.unsound_alloc UnsoundAlloc.transform file

(* transformation based on semantic heuristics *)
let transform : Global.t -> Global.t
= fun global ->
  let loop_transformed = UnsoundLoop.transform global in
  let inlined = Frontend.inline global in
  if not !Options.il && (loop_transformed || inlined) then   (* something transformed *)
    Frontend.makeCFGinfo global.file    (* NOTE: CFG must be re-computed after transformation *)
    |> StepManager.stepf true "Translation to graphs (after inline)" Global.init
    |> StepManager.stepf true "Pre-analysis (after inline)" PreAnalysis.perform
  else global (* nothing changed *)

let front_end_input : unit -> Global.t
= fun () ->
  StepManager.stepf true "Front-end" Frontend.parse ()
  |> Frontend.makeCFGinfo
  |> transform_simple
  |> StepManager.stepf true "Translation to graphs" Global.init

let pre_result : Sparrow_cil.file -> Global.t
= fun file ->
  file
  |> transform_simple
  |> StepManager.stepf true "Translation to graphs" Global.init
  |> StepManager.stepf true "Pre-analysis" PreAnalysis.perform
  |> transform
