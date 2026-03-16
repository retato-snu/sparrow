# Sparrow Abstract Domains and Semantics Analysis

This document organizes the abstract domains and semantic evaluation operators in the Sparrow static analyzer based on the `src/domain` and `src/semantics` directories.

## 1. Abstract Domains (`src/domain/`)

The domains in Sparrow are built compositionally from basic algebraic structures up to complex memory models.

### 1.1 Core Algebraic Signatures (`absDom.mli`)

Sparrow defines its fundamental mathematical structures in `absDom.mli`:
*   **`SET`**: Basic equality, comparison, and string representation.
*   **`CPO` (Complete Partial Order)**: Extends `SET` with partial ordering (`le`), equality (`eq`), bottom element (`bot`), `join`, `meet`, widening (`widen`), and narrowing (`narrow`).
*   **`LAT` (Lattice)**: Extends `CPO` by providing a top element (`top`).

These core signatures are used to ensure that all analysis domains provide the operations needed by the abstract interpreter framework.

### 1.2 Base Domain Constructors

Sparrow uses functors to lift basic domains into more complex structures:
*   **`MapDom`**: Constructs pointwise functional abstractions (maps mapping a `SET` to a `CPO`). It provides `MakeCPO` and `MakeLAT` functors.
*   **`PowDom`**: Constructs powerset domains (sets of elements) from a base `SET`. Also provides `MakeCPO` and `MakeLAT`.

### 1.3 Program-Specific Base Domains

*   **`BasicDom.mli`**: Contains fundamental program types, such as `Node` (control flow graph nodes), `Proc` (procedures), and `Allocsite` (memory allocation sites). 
*   **`Loc` / `PowLoc`**: Represents abstract memory locations (Global vars, Local vars, Allocsites, Fields) and powersets of these locations.

### 1.4 Value Domains

*   **Interval Domain (`itv.mli`, `itvDom.mli`)**: Represents numerical values using intervals bounds (e.g., `[min, max]`). It defines integer arithmetic (`plus`, `minus`, `times`), bitwise operations, and pruning (filtering based on branch conditions).
*   **Taint Domain (`taintDom.mli`)**: Tracks whether values are derived from user input or whether integer overflow can happen. It maintains abstract values as records of `{int_overflow; user_input}`.
*   **Octagon Domain (`octDom.mli`)**: Relational domain tracking relationships between variables such as `±x ±y ≤ c`, implemented via integration with the Apron library.

### 1.5 Memory Domains (`instrumentedMem.mli`)

The memory domain maps abstract locations (`BasicDom.Loc.t` or specialized locations) to abstract values. `InstrumentedMem` extends standard memory with an Access map that records which parts of memory were read and written during evaluation to facilitate sparse analysis. The specific memories are formed by composing `MapDom` with specific locations and value domains (e.g., `ItvDom.Mem.t`, `TaintDom.Mem.t`, `OctDom.Mem.t`).

---

## 2. Semantic Operators (`src/semantics/`)

Semantics modules evaluate Cil expressions and statements over the abstract memory states, producing new states and values.

### 2.1 Abstract Semantics Interface (`absSem.mli`)

`AbsSem.S` defines the signature that every semantics module must implement. Its most critical function is:

```ocaml
val run : update_mode -> Spec.t -> BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t
```
This function takes a node representing a program statement, an abstract state (`Dom.t * Global.t`), and an update mode (Weak or Strong updates) to produce the output state.

### 2.2 Interval Evaluation (`itvSem.mli`)

Evaluates expressions and lvalues based on interval abstraction.
*   `eval_lv`: Evaluates a Cil `lval` to a set of abstract locations (`PowLoc.t`).
*   `eval`: Evaluates a Cil `exp` to an interval value (`ItvDom.Val.t`).
*   `eval_array_alloc` / `eval_string_alloc`: Handles abstract value construction for runtime allocations.

### 2.3 Octagon Evaluation (`octSem.mli`)

Implements semantics for the relational octagon domain. It specifically provides:
*   `check_bo`: Uses relation constraints to evaluate buffer bounds and determine the size interval of an expression, used to check for buffer overflows.

### 2.4 Taint Evaluation (`taintSem.mli`)

Focuses strictly on evaluating `Cil.exp` over the taint state (`TaintDom.Mem.t` and `ItvDom.Mem.t` as a base reference). Traces whether values inherit the `user_input` property.

### 2.5 API Models (`apiSem.ml`)

Since standard library functions (like `memcpy`, `strcpy`, `socket`, `strlen`) cannot be analyzed directly if their source code is missing, `apiSem.ml` models their effects abstractly. It categorizes API arguments based on whether they are sources, destinations, buffers, or sizes, and dictates how their abstract values and taint status transfer from inputs to outputs (`SrcArg`, `DstArg`, `TopWithSrcTaint`, etc.).

---

### Conclusion

Sparrow's domain and semantics architecture is highly modular:
1.  **Algebra (`absDom.mli`)** provides the theoretical correctness laws.
2.  **Constructors (`mapDom.mli`, `powDom.mli`)** build the structural shapes of the analysis state.
3.  **Values (`itv.mli`, `taintDom.mli`, `octDom.mli`)** dictate the precision of single variables.
4.  **Memory (`instrumentedMem.mli`)** glues values to locations, tracking footprint.
5.  **Semantics (`itvSem.mli`, `apiSem.ml`)** act as interpreters, dictating how step-by-step C execution mutates these defined domains.
