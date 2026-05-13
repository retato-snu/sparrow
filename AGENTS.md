---
name: sparrow agent guide
description: Local operational guide for the OCaml Sparrow analyzer and oracle dump path.
audience: contributor / agent
status: active
last-reviewed: 2026-04-27
---

# sparrow

This file contains local operational guidance for the original Sparrow
analyzer. For the exact PE workspace, this directory is both the semantic
authority and the place where deterministic oracle dump/logging support
may be added.

## Local role

| Need                                | Read                                  |
|-------------------------------------|---------------------------------------|
| Exact PE target and module/link pipeline | `../sparrow-exact-pe/Doc/PLAN.md` |
| Oracle dump requirements            | `../sparrow-exact-pe/Doc/ORACLE_REPORT.md` |
| C-fixture acceptance discipline     | `../sparrow-exact-pe/Doc/ABSTRACT_STATE_SCENARIO_COVERAGE.md` |

## Commands

- `dune build`
- `dune exec src/main.exe -- [args]` — run the baseline analyzer on
  a test file; see `how-to-build.md` in this directory for opam switch
  and system-dependency setup.

## Local conventions

- Analyzer semantic changes do not land here for the PE workspace.
  Transfer functions, abstract domains, fixpoint behavior, parser
  semantics, and option semantics are the oracle being compared against.
- Logging, deterministic structured dumps, stable identity printing, and
  command-line oracle output may be added here when required by
  `../sparrow-exact-pe/Doc/ORACLE_REPORT.md`.
- Oracle dump code must be observer-only with respect to analyzer state:
  dump-local numbering or memoization is allowed, but changing transfer
  results, abstract memories, analysis options, or `Global.dump` is not.
- Exact PE implementation work belongs in `../sparrow-exact-pe/`; this
  directory supplies the original semantics and oracle dump executable.
- `src/domain/` is the OCaml-side canonical mathematical domain model.
  When modular implementations cross-reference domain behavior against
  the baseline, this is the authoritative OCaml source.
