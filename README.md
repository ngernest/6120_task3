# CS 6120 Lesson 3: Local Analysis & Optimization

This repo contains an OCaml implementation of the tasks for [Lesson 3](https://www.cs.cornell.edu/courses/cs6120/2025sp/lesson/3/).

## Building the code
- This repo compiles with OCaml 5.1.0 or newer.
- To run the code, the [Opam](https://opam.ocaml.org) package manager is required. Installation instructions for Opam can be found [here](https://opam.ocaml.org/doc/Install.html).
- Run `make install` to install all OCaml dependencies
- Run `make` to compile (under the hood, this invokes the [Dune](https://dune.build/install) build system)
- Run `make test` to run all the QuickCheck tests in this repo 

## Code overview:
- [`syntax.ml`](./lib/syntax.ml): Type definitions for Bril programs + functions for converting from Bril's JSON representation to OCaml types
  - Note: we only support the [*core* Bril instructions](https://capra.cs.cornell.edu/bril/lang/core.html)
- [`lvn.ml`](./lib/lvn.ml): Local value numbering
- [`tdce.ml`](./lib/tdce.ml): Trivial dead code elimination + drop killed instructions
- [`cfg.ml`](./lib/cfg.ml): The algorithm for forming basic blocks & building control flow graphs (from Lesson 2)   
- [`quickcheck_infra.ml`](./lib/quickcheck_infra.ml): QuickCheck generators for Bril types & QC properties for serialization
- [`helpers.ml`](./lib/helpers.ml): Helper functions for dealing with JSON 

## Testing using Turnt + Brench:
The [`bril_tests`](./bril_tests/) subdirectory contains the [DCE tests from the Bril repo](https://github.com/sampsyo/bril/tree/main/examples/test/tdce). 

To test (using [Turnt](https://github.com/cucapra/turnt)) that our transformation doesn't change the observable behavior of any Bril files in the [`bril_tests`](./bril_tests) directory, run:
```bash
$ turnt --diff bril_tests/*.bril
```

The [`benchmarks`](./benchmarks/) subdirectory contains the benchmark files from the main Bril repo. To test using Brench that our transformation correctly 
reduces the no. of dynamic instructions executed in the benchmark files, run:
```bash
$ brench brench.toml
```

## Plotting the optimized results 
We have a Python script ([`analyze_benchmarks.py`](./analyze_benchmarks.py)) which plots the results of each optimization in `plot.png`. The script can be run as follows: 
```bash 
$ uv run analyze_benchmarks.py
```
