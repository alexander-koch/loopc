//! Loop compiler
// Copyright (c) Alexander Koch 2018
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate llvm_sys;
extern crate libc;
extern crate clap;

const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");

use std::fs::File;
use std::io::Read;
use clap::{Arg, App};

pub mod lexer;
pub mod parser;
pub mod codegen;
use lexer::{Lexer, Position, Error};
use parser::Parser;
use parser::*;
use codegen::Codegen;
use codegen::*;

pub fn generate_ast(input: &str) -> Result<ast::Program, Error> {
    // Run the lexer
    let mut lexer = Lexer::new(input);
    let tokens = try!(lexer.run());
    debug!("{:?}", tokens);

    // Generate the AST
    let mut parser = Parser::new(tokens.into_iter());
    parser.parse_program()
}

fn evaluate(path: &str, config: &ProgramConfig) -> Result<(), Error> {
    debug!("Compiling {}...", path);

    // Read the file
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();

    let ast = try!(generate_ast(&content));
    debug!("{:?}", ast);

    // Compile to LLVM code
    let mut codegen = Codegen::new();
    let mut module = try!(codegen.compile(path, config, &ast));
    try!(module.verify()
        .map_err(|x| Error {
            position: Position::new(0,0),
            message: x.into_string().unwrap()
        }));

    // Create an execution engine
    let mut engine = try!(module.create_execution_engine()
            .map_err(|x| Error {
            position: Position::new(0,0),
            message: x.into_string().unwrap()
        }));

    // Print out the source code
    debug!("{}", module.to_cstring().into_string().unwrap());

    // Run the module
    if let Some(val) = engine.run() {
        println!("{}", llvm::generic_value_to_int(val));
    }

    try!(engine.remove_module(&mut module)
            .map_err(|x| Error {
            position: Position::new(0,0),
            message: x.into_string().unwrap()
        }));
    Ok(())
}

fn main() {
    env_logger::init();
    llvm::init_native_jit();

    let matches = App::new("loopc")
        .version(VERSION.unwrap_or("Unknown"))
        .author("Alexander Koch <kochalexander@gmx.net>")
        .about("Just-in-time compiler for loop programs")
        .arg(Arg::with_name("FILE")
            .help("Sets the input file to use")
            .required(true))
        .arg(Arg::with_name("outvar")
            .help("Sets the output variable (default is x1)")
            .required(false)
            .value_name("OUTVAR")
            .takes_value(true)
            .short("o")
            .long("output"))
        .arg(Arg::with_name("input")
            .help("Sets up a start-up program")
            .required(false)
            .value_name("INPUT")
            .takes_value(true)
            .short("i")
            .long("input"))
        .get_matches();

    let prelude = matches.value_of("input")
        .and_then(|x| generate_ast(x).ok());

    let file = matches.value_of("FILE").unwrap();
    let config = ProgramConfig {
        prelude: prelude,
        output: matches.value_of("outvar").unwrap_or("x1")
    };

    if let Err(e) = evaluate(file, &config) {
        println!("{}:{}", file, e)
    }
}
