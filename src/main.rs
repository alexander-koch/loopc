#[macro_use]
extern crate log;
extern crate env_logger;
extern crate llvm_sys;
extern crate libc;

use std::fs::File;
use std::io::Read;

pub mod lexer;
pub mod parser;
pub mod codegen;
use lexer::{Lexer, Position, Error};
use parser::Parser;
use codegen::Codegen;
use codegen::llvm::Module;
use codegen::*;

fn evaluate(path: String) -> Result<(), Error> {
    debug!("Compiling {}...", path);

    // Read the file
    let mut file = File::open(path.clone()).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();

    // Run the lexer
    let mut lexer = Lexer::new(&content);
    let tokens = try!(lexer.run());
    debug!("{:?}", tokens);

    // Generate the AST
    let mut parser = Parser::new(tokens.into_iter());
    let ast = try!(parser.parse_program());
    debug!("{:?}", ast);

    // Compile to LLVM code
    let mut codegen = Codegen::new();
    let mut module = try!(codegen.compile(path, &ast));
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

    // Run the module
    if let Some(val) = engine.run() {
        println!("Result: {}", llvm::generic_value_to_int(val));
    }

    try!(engine.remove_module(&mut module)
            .map_err(|x| Error {
            position: Position::new(0,0),
            message: x.into_string().unwrap()
        }));

    // Print out the source code
    println!("{}", module.to_cstring().into_string().unwrap());
    Ok(())
}

fn main() {
    env_logger::init();
    llvm::init_native_jit();

    if let Err(e) = evaluate("main.loop".into()) {
        println!("main.loop:{}", e)
    }
}
