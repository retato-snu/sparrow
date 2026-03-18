# Sparrow

Sparrow is a state-of-the-art static analyzer that aims to verify the absence
of fatal bugs in C source. Sparrow is designed based on the Abstract Interpretation 
framework and the analysis is sound in design. Sparrow adopts a number of well-founded 
static analysis techniques for scalability, precision, and user convenience.
This is the academic version of Sparrow that is different from the [commercial version][fasoo].

## Build Status
Linux|MAC OSX
-----|-------
[![Build Status](https://ci.appveyor.com/api/projects/status/github/ropas/sparrow?branch=master&svg=true)](https://ci.appveyor.com/api/projects/status/github/ropas/sparrow)|[![Build Status](https://travis-ci.org/ropas/sparrow.svg?branch=master)](https://travis-ci.org/ropas/sparrow)
 
## Sparrow Dependencies
Sparrow requires OCaml >= 4.04.0 and several external libraries. For a complete list of dependencies and detailed installation instructions, please refer to [how-to-build.md](how-to-build.md).

Quick list of OCaml dependencies:
-   [Dune][] >= 2.1
-   [Batteries][] >= 2.3.1
-   [Cil][] (Custom version included as submodule)
-   [Ocamlgraph][] >= 1.8.7
-   [Apron][] >= 0.9.10
-   [Yojson][] >= 1.2.3
-   [Pyml][] >= 1.5.0
-   [Ppx_compare][]
-   [Ppx_deriving][]

## How to Build
The easiest way to build Sparrow is using the provided [how-to-build.md](how-to-build.md) guide.

### Summary of Build Steps
1. Install system prerequisites (GMP, MPFR).
2. Initialize an Opam switch and install dependencies.
3. Initialize the CIL submodule: `git submodule update --init --recursive`.
4. Pin and install the local CIL: `cd cil && opam install . && cd ..`.
5. Build using Dune: `dune build`.
