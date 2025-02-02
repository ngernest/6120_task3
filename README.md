# CS 6120 Lesson 2: Representing Programs 

This repo contains an OCaml implementation of the tasks for [Lesson 2](https://www.cs.cornell.edu/courses/cs6120/2025sp/lesson/2//#tasks).

## Building the code
- This repo compiles with OCaml 5.0.0 or newer.
- To run the code, the [Opam](https://opam.ocaml.org) package manager is required. Installation instructions for Opam can be found [here](https://opam.ocaml.org/doc/Install.html).
- Run `make install` to install all OCaml dependencies
- Run `make` to compile (under the hood, this invokes the [Dune](https://dune.build/install) build system)
- Run `make test` to run all the QuickCheck tests in this repo 

## Code overview:
- [`syntax.ml`](./lib/syntax.ml): Type definitions for Bril programs + functions for converting from Bril's JSON representation to OCaml types
  - Note: we only support the [*core* Bril instructions](https://capra.cs.cornell.edu/bril/lang/core.html) for now
- [`cfg.ml`](./lib/cfg.ml): The algorithm for forming basic blocks & building control flow graphs 
  (translated from the Python code discussed in-class)
- [`add_nops.ml`](./lib/add_nops.ml): Transforms Bril programs by adding a `Nop` after each instruction
- [`quickcheck_properties.ml`](./lib/quickcheck_properties.ml): QuickCheck generators for Bril types & QC properties for CFGs / serialization
- [`helpers.ml`](./lib/helpers.ml): Helper functions for dealing with JSON 

## CFG Algorithm Example:
To test the CFG algorithm on an example Bril program [`jmp.bril`](./bril_tests/jmp.bril), run the following:
```bash 
$ bril2json < bril_tests/jmp.bril | dune exec -- main | dot -Tpdf -o jmp_cfg.pdf
$ open jmp_cfg.pdf
```
This produces a GraphViz visualization of the CFG in [`jmp_cfg.pdf`](./jmp_cfg.pdf).

The basic blocks for this program are: 
```bash
# Basic blocks for `jmp.bril`
b0
  ((Const (v TyInt) (LitInt 4)) (Jmp somewhere))
b1
  ((Const (v TyInt) (LitInt 2)))
somewhere
  ((Print v))
```
Note that the OCaml implementation has found the same basic blocks as the Python program discussed in the [pre-recorded lesson 2 video](https://www.cs.cornell.edu/courses/cs6120/2025sp/lesson/2//#tasks)! 

The CFG algorithm has also been tested on [`br.bril`](./bril_tests/br.bril), whose CFG was discussed in the [pre-recorded videos](https://www.cs.cornell.edu/courses/cs6120/2022sp/lesson/2/), and the output of our algorithm (in [`br_cfg.pdf`](./br_cfg.pdf)) is the same as the CFG in the video. 

We also have some QuickCheck properties ([`quickcheck_properties.ml`](./lib/quickcheck_properties.ml)) which test whether various invariants are maintained during CFG construction.

## Program transformation: Adding Nop after every instruction 
In [`add_nops.ml`](./lib/add_nops.ml), we implement a transformation which adds a [Nop] instruction
after each instruction. This transformation is applied to every function within a Bril program. 

**Testing this tranformation using Turnt**:
The [`bril_tests`](./bril_tests/) subfolder contains all the Bril files for the core Bril interpreter (taken from the [main Bril repo](https://github.com/sampsyo/bril/tree/main/test/interp/core)) (except for the ones where the `main` function takes in non-zero arguments). 

To test (using [Turnt](https://github.com/cucapra/turnt)) that our transformation doesn't change the observable behavior of any Bril files in the [`bril_tests`](./bril_tests) directory, run:
```bash
$ turnt --diff bril_tests/*.bril
```
