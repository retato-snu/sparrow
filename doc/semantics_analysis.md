# Sparrow Abstract Semantics Analysis

This document provides a detailed analysis of the semantic evaluation operators defined in `src/semantics/` for the Sparrow static analyzer.

## 1. Abstract Semantics Signature (`AbsSem.S`)

The core of the abstract interpreter engine in Sparrow relies on the `AbsSem.S` interface (`src/semantics/absSem.mli`). Any semantics module must implement:

```ocaml
module type S = sig
  module Dom : InstrumentedMem.S
  module Spec : Spec.S with type Dom.t = Dom.t ...
  val run : update_mode -> Spec.t -> BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t
  val initial : Dom.PowA.t -> Dom.t
end
```

The `run` function represents the abstract transfer function $\llbracket c \rrbracket^\sharp$. It takes an `update_mode` (indicating whether to perform a Strong update that overwrites values, or a Weak update that joins values), a specification, the current control-flow graph node (`Node.t`), and the input abstract state (memory and global info), and returns the post-state.

## 2. Interval Semantics (`itvSem.ml` / `itvSem.mli`)

`ItvSem` is the foundational semantics engine, executing purely value-based abstract interpretation.

### Key Components:
*   **Expression Evaluation (`eval`)**: Recursively evaluates `Cil.exp` structures into interval values (`ItvDom.Val.t`). It handles integer arithmetic (`eval_bop`), logical operations, unary operations (`eval_uop`), and casts. It also evaluates memory reads (`Lval`).
*   **L-Value Resolution (`eval_lv`)**: Translates a C l-value (`Cil.lval`) into an abstract set of locations (`PowLoc.t`). This is responsible for pointer dereferencing and computing field/array offsets (`resolve_offset`).
*   **Memory Operations (`update`, `lookup`)**: Wraps internal memory functions, checking whether a strong update is sound (e.g., assigning to a concrete, non-recursive local variable vs. a dynamically allocated array or pointer).
*   **Condition Pruning (`prune`)**: Implements narrowing constraints based on branch conditions (e.g., if `x < 10` is true, the interval for `x` in the positive branch is trimmed to `[-∞, 9]`).
*   **Library Modeling**: Contains an extensive list of semantic models for standard C library functions (`strlen`, `strcpy`, `memcpy`, `scanf`, etc.) to simulate their behavior safely overriding undefined function calls in `handle_undefined_functions`.

## 3. Octagon Semantics (`octSem.ml` / `octSem.mli`)

`OctSem` implements relational semantics using the `Apron` numerical library to track linear inequalities among variables (e.g., $x \le y + c$).

### Key Components:
*   **Packs (`PackConf.t`)**: Octagon analysis on all variables is $O(N^3)$, which is too slow. Sparrow uses "packing" to group interacting variables together. `OctSem` evaluates relations specifically within these packs.
*   **Translation to Texpr (`exp_to_texpr`)**: Translates `Cil.exp` standard expressions into Apron's `Texpr1.expr` linear tree expressions. Non-linear operations (like bitwise shifts) are soundly abstracted to `top` (unknown).
*   **Transfer Functions**:
    *   `set`: Assigns an expression to a variable, updating the relational constraints.
    *   `forget`: Removes all relational constraints for a specific variable (used when a variable is overwritten by a non-linear or unanalyzable value).
    *   `prune`: Translates equality/inequality conditions into Apron `Tcons1` constraints to filter the octagon state.
*   **Buffer Overrun Checking (`check_bo`)**: The primary purpose of `OctSem`. It verifies buffer bounds by querying the relations between an array's allocated size constraint and its index expression constraint.

## 4. Taint Semantics (`taintSem.ml` / `taintSem.mli`)

`TaintSem` tracks the flow of user-controlled inputs and verifies potential integer overflows.

### Key Components:
*   **Abstract Value (`Val.t`)**: Composed of `{int_overflow : IntOverflow.t; user_input : UserInput.t}`.
*   **Dependency Tracking (`eval_bop`)**: Taint is propagated fundamentally through `join`s. If `x` is tainted, and `z = x + y`, the `eval_bop` function joins the taint status of `x` and `y`, effectively tainting `z`.
*   **Pointer Aliasing via `ItvSem`**: Taint semantics does not compute pointer targets directly. Instead, when resolving an `Lval` or function call, it relies on the pre-computed points-to information from `ItvSem` (`ItvDom.Table.find node spec.ptrinfo`).
*   **API Semantics Injection (`apiSem.ml`)**: `TaintSem` makes heavy use of `ApiSem` logic to determine if a library function returns a `TaintInput` (like `getchar()`) or propagates taint via arguments (`dst`, `src`, `buf`).

## Conclusion

Sparrow uses a phased semantic approach:
1.  **Interval Semantics** runs first to resolve all points-to relationships, array offsets, and basic value bounds.
2.  **Taint & Octagon Semantics** are subsequent analyses. They do not re-calculate point-to graphs; instead, they "piggyback" on `ItvSem` results (passing `ItvDom.Mem.t` or `spec.ptrinfo` as context) to track specialized properties (relational invariants and information flow).
