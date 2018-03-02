# Loopc

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This is a JIT compiler for the [LOOP programming language](https://en.wikipedia.org/wiki/LOOP_(programming_language)) written in Rust.

## Language

The LOOP language is designed so that it computes only the functions that are primitive recursive.
Therefore, it is not Turing Complete. A program consists of three diffent elements:

* Assignment `xi := xj (+|-) c`
* Sequential execution `P1; P2`
* Loop `loop xi do p end`

## Running programs

To run a program and display its result, you may want to specify input and output variables.
An output variable is simply set by the output flag of the compiler. Inputs can be set using a 'start-up' program.

```sh
$ loopc x.loop -i "x1 := 5; x2 := 4" -o x1
```

The above example will run `x1 := 5; x2 := 4` first and then execute the code contained in `x.loop`.
By omitting the flags, the compiler will default to `x0` as the output variable.
If not set otherwise, variables will be initialized with zero.

## Language extensions

The compiler offers simple language extensions that are not defined in the core language set.
The following extensions are supported:

* Assignment of variables to constants (and arithmetic)
* Assignment of variables to other variables (and arithmetic)
* Multiplication
* Division
* Modulo

You can disable these extensions by setting the `--strict` flag.

## Compiling from source

To compile the source code, a system-wide copy of LLVM must be found within the `PATH`.
The version must be compatible with llvm-sys found in the local [Cargo.toml](Cargo.toml) file.
See https://crates.io/crates/llvm-sys for more info.

If there is a compatible version, just type
```sh
$ cargo build
```

## Contributing

Feel free to file issues and send pull requests.
Contributions are highly welcome!

## License

Copyright (C) 2018 Alexander Koch.
Licensed under [GNU General Public License Version 3](LICENSE).