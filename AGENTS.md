---
name: sparrow agent guide
description: Local operational guide for the OCaml baseline analyzer. Frozen reference per P-7; no new analyzer semantics land here.
audience: contributor / agent
status: active
last-reviewed: 2026-04-22
---

# sparrow

This file contains local operational guidance only. Project principles
live in `../Doc/FOUNDATIONS.md`; process, precedence, and the
architecture map live in `../Doc/OPERATIONS.md`.

## Local role

| Need                                | Read                                  |
|-------------------------------------|---------------------------------------|
| Baseline analyzer role              | `../Doc/OPERATIONS.md` §1.1           |
| Modification scope                  | `../Doc/FOUNDATIONS.md` P-7           |

## Commands

- `dune build`
- `dune exec src/main.exe -- [args]` — run the baseline analyzer on
  a test file; see `how-to-build.md` in this directory for opam switch
  and system-dependency setup.

## Local conventions

- Analyzer semantic changes do not land here. See
  `../Doc/FOUNDATIONS.md` P-7 for the modification scope and for the
  narrow carve-out covering baseline version updates.
- `src/domain/` is the OCaml-side canonical mathematical domain model.
  When the Scala backend cross-references domain behavior against the
  baseline, this is the authoritative OCaml source.
