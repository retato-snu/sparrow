# Sparrow Architecture Overview

Sparrow is a static analyzer for C programs based on abstract interpretation. It leverages a sparse analysis framework to scale up precise memory analysis. The project is structured modularly, mapping directly to typical static analysis pipeline stages from frontend parsing to report generation.

## 1. Core Pipeline & Frontend (`src/core`, `src/program`)

The primary entry point is `core/main.ml`. The overall pipeline of Sparrow is as follows:
1. **Frontend Parsing**: Parses C source code into the CIL (C Intermediate Language) AST using `core/frontend.ml`.
2. **Graph Construction**: Constructs the Control Flow Graph (CFG) and Call Flow Graph (`program/interCfg.ml`, `program/callGraph.ml`). 
3. **Pre-analysis Transformations**: Performs transformations such as unsound loop unrolling or function inlining if specified.
4. **Interval Analysis (ItvAnalysis)**: Runs as the foundational sparse analysis.
5. **Additional Analyses**: Depending on flags, runs subsequent specialized analyses like Taint or Octagon analyses.
6. **Report Generation**: Emits alarms (Buffer Overrun, Null Dereference, etc.) using `report/report.ml`.

The `Global.t` module in `program/global.ml` acts as the central state holder for the file AST, InterCFG, CallGraph, and memory states.

## 2. Abstract Domains & Semantics (`src/domain`, `src/semantics`)

Sparrow parameterizes its analyses by separating abstract domains and semantics.

- **Domains (`src/domain`)**: 
  - `ItvDom.ml`, `TaintDom.ml`, `OctDom.ml`: Define the lattice structures for specific analyses (Intervals, Taint, Octagons).
  - Memory models (`ArrayBlk`, `StructBlk`) track sizes, offsets, and null positions for arrays and structs.

- **Semantics (`src/semantics`)**: 
  - E.g., `itvSem.ml` implements the `AbsSem` interface. It provides evaluation functions (`eval`) for CIL expressions and semantic transfer functions for assignments, function calls, and control flow pruning.
  - It handles sophisticated standard library API modeling (e.g., `malloc`, `strcpy`, `sprintf`) via predefined semantics (`ApiSem`).

## 3. Sparse Analysis Framework (`src/sparse`)

The engine powering the scalability of Sparrow is the Sparse Analysis framework in `sparse/sparseAnalysis.ml`.

Working via standard dataflow equations on a **Def-Use Graph (DUG)** rather than the CFG avoids redundant work:
1. **Access Analysis**: Predicts which memory locations are read or written at each CFG node.
2. **DUG Construction**: Connects definition nodes directly to use nodes, creating a sparse representation (`sparse/ssaDug.ml`).
3. **Fixpoint Iteration**: Uses a worklist algorithm (`sparse/worklist.ml`) to compute the fixpoint of abstract states over the DUG. It employs **widening** to guarantee termination and subsequent **narrowing** to improve precision.

## 4. Instantiation & Reporting (`src/instance`, `src/report`)

- **Analysis Instances (`src/instance`)**:
  - Components like `itvAnalysis.ml` combine the sparse framework functor with the interval semantics (`SparseAnalysis.Make(ItvSem)`). 
  - It configures sensitivities (e.g., partial flow sensitivity) and initiates the fixpoint computation.

- **Query & Reporting (`src/report`)**:
  - After fixpoint iteration, the memory state at each node is queried for potential faults (`report.ml`).
  - Alarm expressions (`AlarmExp.t`) corresponding to Buffer Overruns (BO), Null Dereferences (ND), and Divide-by-Zero (DZ) are collected.
  - Checks (e.g., `check_bo` in `itvAnalysis.ml`) compare allocated array bounds against abstract interval offsets to categorize alarms into `Proven`, `UnProven`, or `BotAlarm`.
