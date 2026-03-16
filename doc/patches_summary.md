# Summary of patches for OCaml 4.14 and Dune Migration

This document details the patches applied to the Sparrow codebase and its CIL dependency to ensure compatibility with OCaml 4.14.2 and successful compilation via Dune.

## 1. Patches for CIL (Safe-String Compatibility)

OCaml 4.02+ introduced `safe-string` (immutable strings by default), which became mandatory in later versions. The following files in CIL were modified to replace mutable string operations with the `Bytes` module.

### [cil/src/formatlex.mll](../cli/src/formatlex.mll)
- **Change**: Replaced `String.set` with `Bytes.set` in the lexer action.
- **Reason**: The internal buffer for formatting strings was being mutated, which is no longer allowed for `string` types.

### [cil/src/frontc/clexer.mll](../cli/src/frontc/clexer.mll)
- **Change**: Converted intermediate string buffers to `Bytes`.
- **Reason**: The lexer used `String.make` and `String.set` to build wide-string literals. This was updated to use `Bytes` and finally `Bytes.to_string`.

### [cil/src/cil.ml](../cli/src/cil.ml)
- **Change**: Replaced `String.copy` and `String.set` in `makeValidSymbolName`.
- **Reason**: CIL modifies symbols to ensure they are valid C identifiers. This now uses `Bytes.of_string` and `Bytes.to_string`.

### [cil/src/ocamlutil/pretty.ml](../cli/src/ocamlutil/pretty.ml)
- **Change**: Replaced `String.set` and formatting operations with `Bytes`.
- **Reason**: The pretty-printing engine mutated strings to manage indentation and alignment.

### [cil/src/ocamlutil/errormsg.ml](../cli/src/ocamlutil/errormsg.ml)
- **Change**: Replaced `String.set` in `rem_backslashes`.
- **Reason**: Internal string cleaning operations were updated for immutability.

## 2. Patches for Sparrow (API and Type Updates)

### 2.1 Yojson API Migration
- **Target**: Multiple files in `src/`.
- **Change**: Replaced `Yojson.Safe.json` with `Yojson.Safe.t`.
- **Reason**: The `json` type constructor was deprecated and removed in recent versions of Yojson in favor of `t`.

### 2.2 Ocamlgraph Wrapper Fix
- **File**: [src/program/intraCfg.ml](../src/program/intraCfg.ml)
- **Change**: Updated the `GDom` module to match the current `Graph.Dominator.I` signature.
- **Reason**:
  - `empty` was changed from a value to a function (`unit -> t`).
  - `add_edge` was updated to return the new graph instead of `unit`, as required by the `ocamlgraph` Persistent graph signature.

### 2.3 Dune Linking and Dependency Fixes
- **File**: [src/dune](../src/dune)
- **Change**: Explicitly listed all CIL subpackages (e.g., `cil.partial`, `cil.ccl`) and required system libraries (`findlib.internal`, `dynlink`, `gmp`).
- **Reason**: Dune's dependency resolution required explicit subpackage names because the project uses advanced CIL features that were failing to link with the simple `cil` package.

## 3. Restoration of Variable Names
During automated type replacement, some variable names (like `json`) were incorrectly replaced by their types (like `Yojson.Safe.t`).
- **Files**: `src/core/vis.ml`, `src/program/interCfg.ml`.
- **Action**: Restored `json`, `chan`, and `l` variable names in function bindings and match statements to fix syntax and unbound value errors.
