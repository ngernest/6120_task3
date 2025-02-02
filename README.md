# CS 6120 Lesson 3: Local Analysis & Optimization

This repo contains a (WIP) OCaml implementation of the tasks for [Lesson 3](https://www.cs.cornell.edu/courses/cs6120/2025sp/lesson/3/).

## Building the code
- This repo compiles with OCaml 5.1.0 or newer.
- To run the code, the [Opam](https://opam.ocaml.org) package manager is required. Installation instructions for Opam can be found [here](https://opam.ocaml.org/doc/Install.html).
- Run `make install` to install all OCaml dependencies
- Run `make` to compile (under the hood, this invokes the [Dune](https://dune.build/install) build system)
- Run `make test` to run all the QuickCheck tests in this repo 

## Code overview:
- [`syntax.ml`](./lib/syntax.ml): Type definitions for Bril programs + functions for converting from Bril's JSON representation to OCaml types
  - Note: we only support the [*core* Bril instructions](https://capra.cs.cornell.edu/bril/lang/core.html)
- [`tdce.ml`](./lib/tdce.ml): Trivial dead code elimination (translated to OCaml from the Python examples in the Bril repo)
- [`cfg.ml`](./lib/cfg.ml): The algorithm for forming basic blocks & building control flow graphs (from Lesson 2)   
- [`quickcheck_infra.ml`](./lib/quickcheck_infra.ml): QuickCheck generators for Bril types & QC properties for serialization
- [`helpers.ml`](./lib/helpers.ml): Helper functions for dealing with JSON 

## Testing using Turnt:
The [`bril_tests`](./bril_tests/) subfolder contains the [DCE tests from the Bril repo](https://github.com/sampsyo/bril/tree/main/examples/test/tdce). 

To test (using [Turnt](https://github.com/cucapra/turnt)) that our transformation doesn't change the observable behavior of any Bril files in the [`bril_tests`](./bril_tests) directory, run:
```bash
$ turnt --diff bril_tests/*.bril
```
