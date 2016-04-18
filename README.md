Goto: considered harmful!
=========================

Goto is a compiler for a subset of Go called GoLite.

Goto has been created in the context of the course COMP 520, taught by Laurie
Hendren at McGill University in the winter 2016 semester.

Team:
* Jacob Errington (260636023)
* Frederic Lafrance (260580705)


Instructions
============

* Install the project dependencies: `cabal install --only-dependencies`
* Build the binary: `cabal build` (prepare to wait a long time)

Two code generators have been implemented.
* (very buggy and incomplete) x86 code generation
* (buggy, but more complete) C++ code generation


x86 native code generation
--------------------------

The x86 code generator requires that the `yasm` assembler be installed.

The x86 code generation will work only for simple integer arithmetic and
control flow. Complex data types such as structs, arrays, or slices do not
work.

In practice, to run the x86 code generator, adjust the variable `ARCH` in
`compile.sh` to reflect your platform:
* Linux systems use `elf64`
* Mac OS X use `macho64`
* Other platforms are unsupported

This script builds the Goto C runtime (`goto.c` and `goto.h` in the `runtime`
directory) using a makefile, and invokes `goto` with the `pretty` subcommand,
which causes x86 code to be output to standard out. This code is saved to a
file and assembled with `yasm` into an ELF object file. The object file is
linked to the Goto runtime into an executable.

Internally, the Go code is translated to an intermediate representation called
VIGIL. The VIGIL code can be dumped to standard out by invoking `goto` with the
`pretty` subcommand and the `--dump-vigil` flag. This flag has no effect when
using other code generators.

C++ code generation
-------------------

The C++ code generator implements pretty much all features. Data structures
such as arrays, slices, and structs are supported.

To run the C++ code generator, use the script `cpp-compile.sh`.

This script will invoke `goto` with the `cpp` subcommand, which causes C++ code
to be written to standard out. This code is saved to a file and compiled with
`g++`, adjusting the header file search path to include the `runtime`
directory. This directory contains the file `goto.hpp` which implements the
Goto C++ runtime. (The C++ runtime is much simpler than the C runtime.)
