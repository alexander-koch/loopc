//! Loop compiler
// Copyright (C) 2018 Alexander Koch
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

extern crate clap;
extern crate env_logger;
extern crate libc;
extern crate llvm_sys;
#[macro_use]
extern crate log;

const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");

use std::fs::File;
use std::io::Read;
use std::io;
use std::fmt;
use std::ffi::CString;
use clap::{App, Arg};

pub mod lexer;
pub mod parser;
pub mod codegen;
use lexer::Lexer;
use parser::Parser;
use codegen::Codegen;
use codegen::*;

#[derive(Debug)]
enum LoopError {
    SyntaxError(lexer::Error),
    IOError(io::Error),
    LLVMError(String),
}

impl From<lexer::Error> for LoopError {
    fn from(err: lexer::Error) -> LoopError {
        LoopError::SyntaxError(err)
    }
}

impl From<io::Error> for LoopError {
    fn from(err: io::Error) -> LoopError {
        LoopError::IOError(err)
    }
}

impl From<CString> for LoopError {
    fn from(err: CString) -> LoopError {
        LoopError::LLVMError(match err.into_string() {
            Ok(s) => s,
            Err(e) => e.to_string(),
        })
    }
}

impl fmt::Display for LoopError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LoopError::SyntaxError(ref x) => fmt::Display::fmt(x, f),
            LoopError::IOError(ref x) => fmt::Display::fmt(x, f),
            LoopError::LLVMError(ref x) => fmt::Display::fmt(x, f),
        }
    }
}

fn generate_ast(input: &str, strict: bool) -> Result<parser::ast::Program, lexer::Error> {
    // Run the lexer
    let mut lexer = Lexer::new(input);
    let tokens = lexer.run()?;
    debug!("{:?}", tokens);

    // Generate the AST
    let mut parser = Parser::new(tokens.into_iter(), strict);
    parser.parse()
}

fn evaluate(path: &str, config: &ProgramConfig) -> Result<(), LoopError> {
    debug!("Compiling {}...", path);

    // Read the file
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let ast = generate_ast(&content, config.strict)?;
    debug!("{:?}", ast);

    // Compile to LLVM code
    let mut codegen = Codegen::new();
    let mut module = codegen.compile(path, config, ast)?;
    module.verify()?;

    // Create an execution engine and run the module
    let mut engine = module.create_execution_engine()?;
    if let Some(val) = engine.run() {
        println!("{}", llvm::generic_value_to_int(val));
    }

    engine.remove_module(&mut module)?;
    Ok(())
}

fn main() {
    env_logger::init();
    llvm::init_native_jit();

    let matches = App::new("loopc")
        .version(VERSION.unwrap_or("Unknown"))
        .author("Alexander Koch <kochalexander@gmx.net>")
        .about("Just-in-time compiler for LOOP programs")
        .arg(
            Arg::with_name("FILE")
                .help("Sets the input file to use")
                .required(true),
        )
        .arg(
            Arg::with_name("outvar")
                .help("Sets the output variable (default is x0)")
                .required(false)
                .value_name("OUTVAR")
                .takes_value(true)
                .short("o")
                .long("output"),
        )
        .arg(
            Arg::with_name("input")
                .help("Sets up a start-up program")
                .required(false)
                .value_name("INPUT")
                .takes_value(true)
                .short("i")
                .long("input"),
        )
        .arg(
            Arg::with_name("strict")
                .help("Enable strict mode")
                .required(false)
                .value_name("STRICT")
                .takes_value(false)
                .short("s")
                .long("strict"),
        )
        .get_matches();

    let strict = matches.occurrences_of("strict") > 0;
    let prelude = matches
        .value_of("input")
        .and_then(|x| generate_ast(x, strict).ok());

    let file = matches.value_of("FILE").unwrap();
    let config = ProgramConfig {
        strict: strict,
        prelude: prelude,
        output: matches.value_of("outvar").unwrap_or("x0"),
    };

    if let Err(e) = evaluate(file, &config) {
        println!("{}:{}", file, e)
    }
}
