# How to Build Sparrow in a Plain Environment

This guide provides step-by-step instructions to set up the environment and build Sparrow from source using the Dune build system.

## 1. System Prerequisites

Sparrow requires several system libraries for its dependencies (like `apron` and `ocamlgraph`). On macOS, you can install these using Homebrew:

```bash
brew install gmp mpfr pkg-config python@3
```

## 2. Opam Setup

Initialize opam and create a new switch for Sparrow:

```bash
opam init
opam switch create sparrow ocaml-base-compiler.4.14.2
eval $(opam env)
```

## 3. Install External Dependencies

Install the required OCaml packages. Note that `cil` is handled as a submodule, so we only install other external libraries:

```bash
opam install . --deps-only --with-test
```

> If `apron` or `pyml` fail to install, ensure that their system dependencies (`gmp`, `python`) are correctly installed and visible to opam.
> Additionally, install the Python dependencies required for the soundness feature:
> ```bash
> pip install -r requirements.txt
> ```
> If you use OCaml5, you need to install pyml manually:
> ```bash
> opam install pyml --update-invariant
> ```

## 4. Submodule Initialization

Sparrow relies on a specific version of CIL located in the `cil/` directory. Ensure it is initialized and updated:

```bash
git submodule update --init --recursive
```

## 5. How to build
First, need to install cil
```bash
cd cil
opam install .
cd ..
```
Then build sparrow
```bash
dune build
```

## 6. How to Use Sparrow

The Sparrow binary is located at `_build/default/src/main.exe`. You can run it on C source files:

```bash
_build/default/src/main.exe [options] source-files
```

### Unsound Feature

If you want to use unsound feature, implmented with python, you need to set `PYML_LIBRARY`, `SPARROW_BIN_PATH` and `SPARROW_DATA_PATH` environment variables.

```bash
PYML_LIBRARY=/path/to/python/lib/python3.*.dylib \
SPARROW_BIN_PATH=$(pwd)/bin \
SPARROW_DATA_PATH=$(pwd)/etc \
dune exec src/main.exe -- -bugfinder 2 test/test.c
```


### Common Parameters

- `-bo`: Enable Buffer-overrun analysis (enabled by default).
- `-nd`: Enable Null-dereference analysis.
- `-oct`: Enable Octagon analysis for numerical relationships.
- `-taint`: Enable Taint analysis.
- `-pfs [0-100]`: Partial flow-sensitivity (0: insensitive, 100: fully flow-sensitive).
- `-verbose [1-5]`: Set verbosity level.
- `-help`: Display all available options.

### Example

To run a buffer-overrun analysis on an example file:

```bash
_build/default/src/main.exe -bo test/test.c
```
or use dune exec:
```bash
dune exec src/main.exe -- -bo test/test.c
``` 

To run a numerical analysis using octagons with full flow-sensitivity:

```bash
_build/default/src/main.exe -oct -pfs 100 test/test.c
```
or use dune exec:
```bash
dune exec src/main.exe -- -oct -pfs 100 test/test.c
```

## Troubleshooting

- **CIL Linkage Issues**: If you encounter errors related to CIL subpackages, verify that `src/dune` explicitly lists them (e.g., `cil.partial`).
- **Yojson Errors**: Ensure you are using a recent version of Yojson that supports the `Yojson.Safe.t` type (version 1.6.0 or later).
- **File Not Found**: Ensure you are running the command from the Sparrow root directory or provide absolute paths to the source files.
