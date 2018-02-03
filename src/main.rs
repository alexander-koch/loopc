#[macro_use]
extern crate log;
extern crate env_logger;

use std::fs::File;
use std::io::Read;

pub mod lexer;
pub mod parser;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let path = "main.loop";
    println!("Compiling {}...", path);

    // Read the file
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();

    // Run the lexer
    let mut lexer = Lexer::new(&content);
    let tokens = lexer.run();
    if let Ok(token_vec) = tokens {
        println!("{:?}", token_vec);

        let mut parser = Parser::new(token_vec.into_iter());
        let ast = parser.parse_program();
        println!("{:?}", ast);
    }
}
