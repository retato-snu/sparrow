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
open Graph
open Sparrow_cil
open Global
open BasicDom
open Vocab
open Frontend
open IntraCfg
open ItvDom
open ArrayBlk
open AlarmExp
open Report

module Analysis = SparseAnalysis.Make(ItvSem)
module Table = Analysis.Table
module Spec = Analysis.Spec

let print_abslocs_info locs =
  let lvars = BatSet.filter Loc.is_lvar locs in
  let gvars = BatSet.filter Loc.is_gvar locs in
  let allocsites = BatSet.filter Loc.is_allocsite locs in
  let fields = BatSet.filter Loc.is_field locs in
    prerr_endline ("#abslocs    : " ^ i2s (BatSet.cardinal locs));
    prerr_endline ("#lvars      : " ^ i2s (BatSet.cardinal lvars));
    prerr_endline ("#gvars      : " ^ i2s (BatSet.cardinal gvars));
    prerr_endline ("#allocsites : " ^ i2s (BatSet.cardinal allocsites));
    prerr_endline ("#fields     : " ^ i2s (BatSet.cardinal fields))

(* **************** *
 * Alarm Inspection *
 * **************** *)
let ignore_alarm a arr offset =
  (!Options.bugfinder >= 1
    && (Allocsite.is_string_allocsite a
       || arr.ArrInfo.size = Itv.top
       || arr.ArrInfo.size = Itv.one
       || offset = Itv.top && arr.ArrInfo.size = Itv.nat
       || offset = Itv.zero))
  || (!Options.bugfinder >= 2
      && not (Itv.is_const arr.ArrInfo.size))
  || (!Options.bugfinder >= 3
       && (offset = Itv.top
          || Itv.meet arr.ArrInfo.size Itv.zero <> Itv.bot
          || (offset = Itv.top && arr.ArrInfo.offset <> Itv.top)))


let check_bo v1 v2opt : (status * Allocsite.t option * string) list =
  let arr = Val.array_of_val v1 in
  if ArrayBlk.eq arr ArrayBlk.bot then [(BotAlarm, None, "Array is Bot")] else
    ArrayBlk.foldi (fun a arr lst ->
      let offset =
        match v2opt with
        | None -> arr.ArrInfo.offset
        | Some v2 -> Itv.plus arr.ArrInfo.offset (Val.itv_of_val v2) in
      let status =
        try
          if Itv.is_bot offset || Itv.is_bot arr.ArrInfo.size then BotAlarm
          else if ignore_alarm a arr offset then Proven
          else
            let (ol, ou) = (Itv.lower offset, Itv.upper offset) in
            let sl = Itv.lower arr.ArrInfo.size in
            if ou >= sl || ol < 0 then UnProven
            else Proven
        with _ -> UnProven
      in
      (status, Some a, string_of_alarminfo offset arr.ArrInfo.size)::lst
    ) arr []

let check_nd v1 : (status * Allocsite.t option * string) list =
  let ploc = Val.pow_loc_of_val v1 in
  if PowLoc.eq ploc PowLoc.bot then [(BotAlarm, None, "PowLoc is Bot")] else
    if PowLoc.mem Loc.null ploc then
      [(UnProven, None, "Null Dereference")]
    else [(Proven, None, "")]

(* MODULAR EXTERNAL-DEREF FLOOR (Options.modular_extern_deref_floor, default OFF -> oracle unchanged).
   The saturating external residual value for a dereference/index base the modular union lost to bot:
   an external ARRAY over a fresh ext allocsite (ArrayBlk.extern -> size TOP, so a BO index is UnProven)
   JOINED with a NULL points-to (so a ND deref is UnProven "Null Dereference", never a vacuous proof).
   This is EXACTLY the sound residual-TOP of a genuinely-open input (Boundary Fidelity); it can only
   turn a bot base into an honest UnProven, never into a false Proven and never weaken a real proof. *)
let extern_deref_floor_val : Val.t =
  Val.join (Val.of_pow_loc (PowLoc.singleton Loc.null))
    (Val.external_value (Allocsite.allocsite_of_ext None))

(* Floor a deref/index BASE value [v] when the component the alarm check reads is bot at a REACHABLE
   node.  The [mem] passed here is the node's INPUT memory (generate: Table.find node inputof), and
   generate already filters dead code (mem = Mem.bot -> skipped), so reaching this point means the node
   EXECUTES in the union's dataflow.  A base whose ARRAY block is bot (for a BO index/deref) or whose
   POINTS-TO is bot (for an ND deref) means the base is a value the per-module residual could not
   reconstruct (an undefined-library return / an unresolved cross-module global or struct field) -- the
   genuine external-source under-approximation the whole-program oracle raises.  CRUCIALLY it is NOT
   enough to test `v = bot`: a residual pointer can have a non-bot POINTS-TO yet a BOT ARRAY block (so
   the BO "Array is Bot" is vacuously rewritten to a Proven "valid pointer dereference"), or a non-bot
   interval yet a BOT points-to (so the ND deref is vacuously Proven) -- both are the SAME dropped alarm.
   We floor per-check-component so the check runs against the saturating residual TOP (UnProven).  A base
   with a CONCRETE array / points-to is left untouched (a genuine local proof is preserved).  Guarded by
   the modular-only flag; a byte-for-byte no-op for the whole-program oracle (flag OFF).
   [kind]: `BO tests the array block, `ND tests the points-to set. *)
let extern_deref_floor kind _node _mem v =
  if not !Options.modular_extern_deref_floor then v
  else
    let component_bot =
      match kind with
      | `BO -> ArrayBlk.eq (Val.array_of_val v) ArrayBlk.bot
      (* ND: a points-to-bot deref base is a vacuous ND proof (check_nd rewrites "PowLoc is Bot" ->
         Proven "valid pointer dereference").  Flooring EVERY such base to may-null is sound but very
         costly (vacuous ND proofs are common), so restrict the ND floor to the base being ENTIRELY bot
         (the genuine external-source case, where nothing at all was reconstructed) unless the caller
         opts into the broader powloc-bot floor with UNION_ND_POWLOC_FLOOR=1.  The BO (array) floor has
         no such cost (it only fires where the deref/index would otherwise vacuously prove) so it stays
         component-aware. *)
      | `ND ->
        if Sys.getenv_opt "UNION_ND_POWLOC_FLOOR" <> None
        then PowLoc.eq (Val.pow_loc_of_val v) PowLoc.bot
        else Val.eq v Val.bot
    in
    if component_bot then Val.join v extern_deref_floor_val else v

(* Floor a bot INDEX / SIZE operand (the [Some v2] of check_bo: an array index `arr[i]`, or the
   length arg of a memcpy/memmove) to Itv.top.  When the base array is a CONCRETE global (e.g. wget's
   `_sch_istable` char-class table) but the index `((int)*p)&0xff` traces to an externally-sourced
   deref `*p` that the union lost to bot, the index value is bot -- and `check_bo` computes
   `offset = Itv.plus base.offset bot = bot` (bot annihilates under Itv.plus), yielding a spurious
   "Array is Bot"/BotAlarm that SUPPRESSES the oracle's real BO alarm.  Flooring the bot index/size to
   Itv.top makes `offset = base.offset + TOP = TOP` -> UnProven, the sound direction (an unknown
   external index can land anywhere).  Modular-only; a no-op for the oracle. *)
let extern_index_floor v2opt =
  match v2opt with
  | Some v2 when !Options.modular_extern_deref_floor
                 && Itv.is_bot (Val.itv_of_val v2) ->
    Some (Val.of_itv Itv.top)
  | _ -> v2opt

let inspect_aexp_bo : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  (match aexp with
    | ArrayExp (lv,e,loc) ->
        let v1 = Mem.lookup (ItvSem.eval_lv (InterCfg.Node.get_pid node) lv mem) mem in
        let v1 = extern_deref_floor `BO node mem v1 in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
        let lst = check_bo v1 (extern_index_floor (Some v2)) in
        List.map (fun (status,a,desc) ->
          { node = node; exp = aexp; loc = loc; allocsite = a;
            status = status; desc = desc; src = None }) lst
    | DerefExp (e,loc) ->
        let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
        let v = extern_deref_floor `BO node mem v in
        let lst = check_bo v None in
          if Val.eq Val.bot v then
            List.map (fun (status,a,desc) ->
              { node = node; exp = aexp; loc = loc; allocsite = a;
                status = status; desc = desc; src = None }) lst
          else
            (* [deref_offset_annihilated]: the base carries a NON-bot ARRAY block WITH A NON-bot SIZE, but
               check_bo still yields BotAlarm because the OFFSET alone is bot.  A DerefExp `*(base+idx)`
               folds the INDEX into the evaluated pointer, so there is no separate v2 for extern_index_floor
               to lift; when the folded index traces to an externally-sourced value the union lost to bot (a
               cross-module table `@functions+new_func`, new_func a bot cross-module counter), or a concrete
               local/global array indexed by such a value, `Itv.plus offset bot = bot` (bot annihilates) ->
               BotAlarm "offset: bot" while the SIZE stays concrete, and the default rewrite below vacuously
               PROVES the deref -- the NO-QUERY BO under-approximation the whole-program oracle raises (which
               it flags UnProven against the same concrete array).  So emit an honest UnProven with offset
               TOP (the sound residual of an unknown external index) against the array's own size.
               Restricting to a NON-bot SIZE is what keeps this from over-firing: a degenerate residual
               pointer whose ARRAY exists but whose SIZE is also bot (e.g. an unresolved `*p` output param,
               argcp/argvp/ifstat) is NOT a concrete-array index the oracle proves-vs -- the oracle leaves
               those Proven -- so flooring them would be a spurious alarm; those keep the vacuous-Proven path.
               Modular-only + size-gated, so the whole-program oracle is byte-for-byte unchanged. *)
            let arr = Val.array_of_val v in
            let deref_offset_annihilated =
              !Options.modular_extern_deref_floor
              && Sys.getenv_opt "UNION_NO_DEREF_OFFSET_FLOOR" = None
              && not (ArrayBlk.eq arr ArrayBlk.bot)
              && not (Itv.is_bot (ArrayBlk.sizeof arr)) in
            (* FIX (c) BO-VACUOUS-SIZE floor: a deref `*(base+idx)` whose base ARRAY BLOCK EXISTS but
               whose SIZE is bot -- the residual of an externally-sourced LENGTH the union lost (make
               `value = alloca(strlen(v->value)+..)` with `v->value` a cross-module heap field -> its
               strlen bots -> the alloca size bots; wget append_url/header_process, an external length).
               check_bo returns BotAlarm "Array is Bot"/"size bot" and the default rewrite vacuously
               PROVES -- the SAME under-approximation as the concrete-size annihilation above, one size
               level down.  The whole-program oracle sizes the alloca against the same allocsite (size
               [2,+oo]) and raises UnProven, so the union's vacuous proof is a genuine BO under-approx.
               Floor it to an honest UnProven with offset+size TOP (the sound residual of an unknown
               external length).  DISTINCT from the annihilation gate: this ALSO fires when the SIZE is
               bot, so it recovers the argcp/argvp-shaped cases the size-gate intentionally left Proven.
               The comment above warns those are precision-costly (an unresolved `*p` output param the
               oracle leaves Proven); this floor is therefore SEPARATELY gated (default ON, opt out with
               UNION_NO_DEREF_SIZEBOT_FLOOR) and its precision cost is measured in the fit audit.  SOUND:
               it only turns a vacuous BotAlarm proof into an honest UnProven, never a real proof into an
               alarm and never an alarm into a proof. *)
            let deref_size_bot =
              !Options.modular_extern_deref_floor
              && Sys.getenv_opt "UNION_NO_DEREF_OFFSET_FLOOR" = None
              && Sys.getenv_opt "UNION_NO_DEREF_SIZEBOT_FLOOR" = None
              && not (ArrayBlk.eq arr ArrayBlk.bot)
              && Itv.is_bot (ArrayBlk.sizeof arr) in
            List.map (fun (status,a,desc) ->
              if status = BotAlarm && deref_offset_annihilated
              then { node = node; exp = aexp; loc = loc; status = UnProven; allocsite = a;
                     desc = string_of_alarminfo Itv.top (ArrayBlk.sizeof arr);
                     src = None }
              else if status = BotAlarm && deref_size_bot
              then { node = node; exp = aexp; loc = loc; status = UnProven; allocsite = a;
                     desc = string_of_alarminfo Itv.top Itv.top;
                     src = None }
              else if status = BotAlarm
              then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a;
                     desc = "valid pointer dereference"; src = None }
              else { node = node; exp = aexp; loc = loc; status = status; allocsite = a;
                     desc = desc; src = None }) lst
    | Strcpy (e1, e2, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in
        let v2 = Val.of_itv (ArrayBlk.nullof (Val.array_of_val v2)) in
        let lst = check_bo v1 (Some v2) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a;
                                           status = status; desc = desc; src = None }) lst
    | Strcat (e1, e2, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in
        let np1 = ArrayBlk.nullof (Val.array_of_val v1) in
        let np2 = ArrayBlk.nullof (Val.array_of_val v2) in
        let np = Val.of_itv (Itv.plus np1 np2) in
        let lst = check_bo v1 (Some np) in
        List.map (fun (status,a,desc) -> {
              node = node; exp = aexp; loc = loc; allocsite = a;
              status = status; desc = desc; src = None }) lst
    | Strncpy (e1, e2, e3, loc)
    | Memcpy (e1, e2, e3, loc)
    | Memmove (e1, e2, e3, loc) ->
        let v1 = extern_deref_floor `BO node mem (ItvSem.eval (InterCfg.Node.get_pid node) e1 mem) in
        let v2 = extern_deref_floor `BO node mem (ItvSem.eval (InterCfg.Node.get_pid node) e2 mem) in
        let e3_1 = Sparrow_cil.BinOp (Sparrow_cil.MinusA, e3, Sparrow_cil.mone, Sparrow_cil.intType) in
        let v3 = ItvSem.eval (InterCfg.Node.get_pid node) e3_1 mem in
        let v3opt = extern_index_floor (Some v3) in
        let lst1 = check_bo v1 v3opt in
        let lst2 = check_bo v2 v3opt in
        List.map (fun (status,a,desc) ->
            { node = node; exp = aexp; loc = loc; allocsite = a;
              status = status; desc = desc; src = None }) (lst1@lst2)
    | _ -> []) @ queries

let inspect_aexp_nd : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  (match aexp with
  | DerefExp (e,loc) ->
    let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
    let v = extern_deref_floor `ND node mem v in
    let lst = check_nd v in
      if Val.eq Val.bot v then
        List.map (fun (status,a,desc) ->
            { node = node; exp = aexp; loc = loc; allocsite = a;
              status = status; desc = desc; src = None }) lst
      else
        List.map (fun (status,a,desc) ->
          if status = BotAlarm
          then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a;
                 desc = "valid pointer dereference"; src = None }
          else { node = node; exp = aexp; loc = loc; status = status; allocsite = a;
                 desc = desc; src = None }) lst
  | _ -> []) @ queries

let check_dz v =
  let v = Val.itv_of_val v in
  if Itv.le Itv.zero v then
    [(UnProven, None, "Divide by "^Itv.to_string v)]
  else [(Proven, None, "")]

let inspect_aexp_dz : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
= fun node aexp mem queries ->
  (match aexp with
      DivExp (_, e, loc) ->
      let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
      let lst = check_dz v in
        List.map (fun (status,a,desc) ->
          { node = node; exp = aexp; loc = loc; allocsite = None;
            status = status; desc = desc; src = None }) lst
  | _ -> []) @ queries

let machine_gen_code : query -> bool
= fun q ->
  (* yacc-generated code *)
  Filename.check_suffix q.loc.Sparrow_cil.file ".y" || Filename.check_suffix q.loc.Sparrow_cil.file ".yy.c" ||
  Filename.check_suffix q.loc.Sparrow_cil.file ".simple" ||
  (* sparrow-generated code *)
  InterCfg.Node.get_pid q.node = InterCfg.global_proc

let rec unsound_exp : Sparrow_cil.exp -> bool
= fun e ->
  match e with
  | Sparrow_cil.BinOp (Sparrow_cil.PlusPI, Sparrow_cil.Lval (Sparrow_cil.Mem _, _), _, _) -> true
  | Sparrow_cil.BinOp (b, _, _, _) when b = Mod || b = Sparrow_cil.Shiftlt || b = Shiftrt || b = BAnd
      || b = BOr || b = BXor || b = LAnd || b = LOr -> true
  | Sparrow_cil.BinOp (bop, Sparrow_cil.Lval (Sparrow_cil.Var _, _), Sparrow_cil.Lval (Sparrow_cil.Var _, _), _)
    when bop = Sparrow_cil.PlusA || bop = Sparrow_cil.MinusA -> true
  | Sparrow_cil.BinOp (_, e1, e2, _) -> (unsound_exp e1) || (unsound_exp e2)
  | Sparrow_cil.CastE (_, _, e) -> unsound_exp e
  | Sparrow_cil.Lval lv -> unsound_lv lv
  | _ -> false

and unsound_lv : Sparrow_cil.lval -> bool = function
  | (_, Sparrow_cil.Index _) -> true
  | (Sparrow_cil.Var v, _) -> is_global_integer v || is_union v.vtype || is_temp_integer v
  | (Sparrow_cil.Mem _, Sparrow_cil.NoOffset) -> true
  | (_, _) -> false
and is_global_integer v = v.vglob && Sparrow_cil.isIntegralType v.vtype
and is_union typ =
  match Sparrow_cil.unrollTypeDeep typ with
    Sparrow_cil.TPtr (Sparrow_cil.TComp (c, _), _) -> not c.cstruct
  | _ -> false
and is_temp_integer v =
  !Options.bugfinder >= 2
  && (try String.sub v.vname 0 3 = "tmp" with _ -> false)
  && Sparrow_cil.isIntegralType v.vtype

let unsound_aexp : AlarmExp.t -> bool = function
  | ArrayExp (lv, e, _) -> unsound_exp e
  | DerefExp (e, _) -> unsound_exp e
  | _ -> false

let formal_param : Global.t -> query -> bool
= fun global q ->
  let cfg = InterCfg.cfgof global.icfg (InterCfg.Node.get_pid q.node) in
  let formals = IntraCfg.get_formals cfg |> List.map (fun x -> x.Sparrow_cil.vname) in
  let rec find_exp = function
    | Sparrow_cil.BinOp (_, e1, e2, _) -> (find_exp e1) || (find_exp e2)
    | Sparrow_cil.CastE (_, _, e) -> find_exp e
    | Sparrow_cil.Lval lv -> find_lv lv
    | _ -> false
  and find_lv = function
    | (Sparrow_cil.Var v, _) -> (List.mem v.vname formals) && Sparrow_cil.isIntegralType v.vtype
    | (_, _) -> false
  in
  match q.exp with
  | ArrayExp (_, e, _) | DerefExp (e, _) -> find_exp e
  | _ -> false

let unsound_filter : Global.t -> query list -> query list
= fun global ql ->
  let filtered =
    List.filter (fun q ->
      not (machine_gen_code q)
      && not (unsound_aexp q.exp)
(*     not (formal_param global q)*)) ql
  in
  let partition =
    list_fold (fun q m ->
      let p_als = try BatMap.find (q.loc,q.node) m with _ -> [] in
        BatMap.add (q.loc,q.node) (q::p_als) m
    ) filtered BatMap.empty
  in
  BatMap.fold (fun ql result ->
      if List.length (Report.get ql UnProven) > 3 then
        (List.map (fun q -> { q with status = Proven}) ql)@result
      else ql@result) partition []

let filter : query list -> status -> query list
= fun qs s -> List.filter (fun q -> q.status = s) qs

let generate : Global.t * Table.t * target -> query list
=fun (global,inputof,target) ->
  let nodes = InterCfg.nodesof global.icfg in
  let total = List.length nodes in
  list_fold (fun node (qs,k) ->
    prerr_progressbar ~itv:1000 k total;
    let mem = Table.find node inputof in
    let cmd = InterCfg.cmdof global.icfg node in
    let aexps = AlarmExp.collect cmd in
    let qs = list_fold (fun aexp ->
      if mem = Mem.bot then id (* dead code *)
      else
        match target with
          BO -> inspect_aexp_bo node aexp mem
        | ND -> inspect_aexp_nd node aexp mem
        | DZ -> inspect_aexp_dz node aexp mem
      ) aexps qs
    in
    (qs, k+1)
  ) nodes ([],0)
  |> fst
  |> opt (!Options.bugfinder > 0) (unsound_filter global)

let generate_with_mem : Global.t * Mem.t * target -> query list
=fun (global,mem,target) ->
  let nodes = InterCfg.nodesof global.icfg in
    list_fold (fun node ->
      let cmd = InterCfg.cmdof global.icfg node in
      let aexps = AlarmExp.collect cmd in
        if mem = Mem.bot then id (* dead code *)
        else
          match target with
            BO -> list_fold (fun aexp  -> inspect_aexp_bo node aexp mem) aexps
          | ND -> list_fold (fun aexp  -> inspect_aexp_nd node aexp mem) aexps
          | DZ -> list_fold (fun aexp  -> inspect_aexp_dz node aexp mem) aexps
    ) nodes []

(* ********** *
 * Marshaling *
 * ********** *)

let marshal_in : Global.t -> Global.t * Table.t * Table.t
= fun global ->
  let filename = Filename.basename global.file.fileName in
  let global = MarshalManager.input (filename ^ ".itv.global") in
  let input = MarshalManager.input (filename ^ ".itv.input") in
  let output = MarshalManager.input (filename ^ ".itv.output") in
  (global,input,output)

let marshal_out : Global.t * Table.t * Table.t -> Global.t * Table.t * Table.t
= fun (global,input,output) ->
  let filename = Filename.basename global.file.fileName in
  MarshalManager.output (filename ^ ".itv.global") global;
  MarshalManager.output (filename ^ ".itv.input") input;
  MarshalManager.output (filename ^ ".itv.output") output;
  (global,input,output)

let inspect_alarm : Global.t -> Spec.t -> Table.t -> Report.query list
= fun global _ inputof ->
  (if !Options.bo then generate (global,inputof,Report.BO) else [])
  @ (if !Options.nd then generate (global,inputof,Report.ND) else [])
  @ (if !Options.dz then  generate (global,inputof,Report.DZ) else [])

let get_locset mem =
  ItvDom.Mem.foldi (fun l v locset ->
    locset
    |> PowLoc.add l
    |> PowLoc.union (Val.pow_loc_of_val v)
    |> BatSet.fold (fun a -> PowLoc.add (Loc.of_allocsite a)) (Val.allocsites_of_val v)
  ) mem PowLoc.empty

let do_analysis : Global.t -> Global.t * Table.t * Table.t * Report.query list
= fun global ->
  (* whole-program analysis builds its values from one CIL parse, so equal values
     are physically shared and value hash-consing is the intended memory win.  Make
     sure it is ON even if a modular link earlier in this process disabled it (the
     modular separate-compilation link turns it off because Marshaled artifacts break
     the sharing -- see mapDom.mli / modular_core link). *)
  Mem.set_b_hashcons true;
  Table.set_b_hashcons true;
  Dump.set_b_hashcons true;
  let _ = prerr_memory_usage () in
  let locset = get_locset global.mem in
  let locset_fs = PartialFlowSensitivity.select global locset in
  let unsound_lib = UnsoundLib.collect global in
  let unsound_update = (!Options.bugfinder >= 2) in
  let unsound_bitwise = (!Options.bugfinder >= 1) in
  let spec = { Spec.empty with
    Spec.locset; Spec.locset_fs; premem = global.mem; Spec.unsound_lib;
    Spec.unsound_update; Spec.unsound_bitwise; } in
  cond !Options.marshal_in marshal_in (Analysis.perform spec) global
  |> opt !Options.marshal_out marshal_out
  |> StepManager.stepf true "Generate Alarm Report" (fun (global,inputof,outputof) ->
      (global,inputof,outputof,inspect_alarm global spec inputof))

let do_analysis_with_sparse_transfer_hook hook global =
  let _ = prerr_memory_usage () in
  let locset = get_locset global.mem in
  let locset_fs = PartialFlowSensitivity.select global locset in
  let unsound_lib = UnsoundLib.collect global in
  let unsound_update = (!Options.bugfinder >= 2) in
  let unsound_bitwise = (!Options.bugfinder >= 1) in
  let spec = { Spec.empty with
    Spec.locset; Spec.locset_fs; premem = global.mem; Spec.unsound_lib;
    Spec.unsound_update; Spec.unsound_bitwise; } in
  let transfer_scope _node f = ItvSem.with_transfer_hook hook f in
  cond !Options.marshal_in marshal_in
    (Analysis.perform_with_transfer_scope transfer_scope spec) global
  |> opt !Options.marshal_out marshal_out
  |> StepManager.stepf true "Generate Alarm Report" (fun (global,inputof,outputof) ->
      (global,inputof,outputof,inspect_alarm global spec inputof))

let do_analysis_with_sparse_analysis_hook ?seed_closed analysis_hook transfer_hook global =
  let _ = prerr_memory_usage () in
  let locset = get_locset global.mem in
  let locset_fs = PartialFlowSensitivity.select global locset in
  let unsound_lib = UnsoundLib.collect global in
  let unsound_update = (!Options.bugfinder >= 2) in
  let unsound_bitwise = (!Options.bugfinder >= 1) in
  let spec = { Spec.empty with
    Spec.locset; Spec.locset_fs; premem = global.mem; Spec.unsound_lib;
    Spec.unsound_update; Spec.unsound_bitwise; } in
  let transfer_scope _node f = ItvSem.with_transfer_hook transfer_hook f in
  cond !Options.marshal_in marshal_in
    (Analysis.perform_with_scopes ?seed_closed transfer_scope analysis_hook spec) global
  |> opt !Options.marshal_out marshal_out
  |> StepManager.stepf true "Generate Alarm Report" (fun (global,inputof,outputof) ->
      (global,inputof,outputof,inspect_alarm global spec inputof))
