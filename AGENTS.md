# sparrow

## Role
This directory contains the legacy (Sprint -1) baseline global static analyzer developed in OCaml. It encompasses heavyweight semantic analytical passes, flow sensitivities, memory graphs, and null-dereference capabilities.

## Architectural Rules

1. **Non-Modular Design:** This executes global analyses on monolithic graphs. Do NOT modify the behavior of the modular staged components from this directory. 
2. **Coupling Warning:** Historically, standalone features (e.g., AST JSON extraction) were crammed into this monolithic entrypoint (`src/core/main.ml`), entangling lightweight tools with heavyweight analytic dependencies. Keep decoupled execution utilities completely separate from `sparrow` (e.g. `sparrow-dumper/`).
3. **Reference Material:** When rebuilding global properties into staged Scala constraints (Sprint 1+), `sparrow/src/domain/` objects often serve as the mathematical domain model and ground truth to cross-reference against.
