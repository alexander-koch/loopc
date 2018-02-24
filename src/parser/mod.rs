//! Syntax analysis.
//! Generates an abstract syntax tree based on a vector of tokens.
// Copyright (c) Alexander Koch 2018
use std::iter::Peekable;
use lexer::{Token, TokenType, Position, Keyword, Error};

pub mod ast;
pub type ParsingResult<T> = ::std::result::Result<T, Error>;

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    current: Token
}

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Creates a new parser based on an iterator of tokens.
    pub fn new(it: T) -> Parser<T> {
        let mut peek = it.peekable();
        let start = peek.next().unwrap_or(Token {
            typ: TokenType::Eof,
            value: None,
            position: Position::new(-1, -1),
        });
        Parser {
            tokens: peek,
            current: start,
        }
    }

    /// Generates an error object.
    ///
    /// # Arguments
    /// * `message` - Error message do display
    fn err(&self, message: &str) -> Error {
        Error {
            message: message.into(),
            position: self.current.position,
        }
    }

    /// 'Bumps' the parser to read the next token.
    /// If there is no token, Token EOF is returned.
    fn bump(&mut self) {
        self.current = self.tokens.next().unwrap_or(Token {
            typ: TokenType::Eof,
            value: None,
            position: Position::new(-1, -1),
        })
    }

    /// If the current token contains a string value it is returned.
    /// Otherwise an empty string is returned.
    fn get_current_value(&mut self) -> String {
        String::from(match self.current.value {
            Some(ref v) => &**v,
            None => "",
        })
    }

    fn expect_type(&mut self, t: TokenType) -> ParsingResult<()> {
        if self.current.typ == t {
            self.bump();
            Ok(())
        } else {
            let msg = format!("Unexpected token `{:?}`, expected: `{:?}`",
                              self.current.typ,
                              t);
            Err(self.err(msg.as_str()))
        }
    }

    fn expect_one_of_types(&mut self, tv: Vec<TokenType>) -> ParsingResult<()> {
        if tv.contains(&self.current.typ) {
            self.bump();
            Ok(())
        } else {
            let msg = format!("Unexpected token `{:?}`, expected one of: `{:?}`",
                              self.current.typ,
                              tv);
            Err(self.err(msg.as_str()))
        }
    }

    pub fn parse_loop(&mut self) -> ParsingResult<ast::Program> {
        debug!("Parsing: loop");
        let position = self.current.position;
        try!(self.expect_type(TokenType::Keyword(Keyword::Loop)));

        let ident = self.get_current_value();
        try!(self.expect_type(TokenType::Identifier));
        try!(self.expect_type(TokenType::Keyword(Keyword::Do)));
        let program = try!(self.parse_program());
        try!(self.expect_type(TokenType::Keyword(Keyword::End)));

        Ok(ast::Program {
            position: position,
            kind: ast::ProgramKind::Loop(ident, Box::new(program))
        })
    }


    pub fn parse_assignment(&mut self) -> ParsingResult<ast::Program> {
        debug!("Parsing: assignment");
        let position = self.current.position;
        let assignee = self.get_current_value();
        try!(self.expect_type(TokenType::Identifier));
        try!(self.expect_type(TokenType::Assignment));

        let lhs = self.get_current_value();
        try!(self.expect_one_of_types(vec![TokenType::Identifier, TokenType::Constant]));

        let operator = match self.current.typ {
            TokenType::Plus => ast::BinaryOperator::Plus,
            TokenType::Minus => ast::BinaryOperator::Minus,
            _ => {
                return Ok(ast::Program {
                    position: position,
                    kind: ast::ProgramKind::Assignment(assignee,
                            lhs,
                            ast::BinaryOperator::Nop,
                            "".to_owned())
                })
            }
        };
        self.bump();

        let rhs = self.get_current_value();
        try!(self.expect_one_of_types(vec![TokenType::Identifier, TokenType::Constant]));

        Ok(ast::Program {
            position: position,
            kind: ast::ProgramKind::Assignment(assignee, lhs, operator, rhs)
        })
    }

    pub fn parse_program(&mut self) -> ParsingResult<ast::Program> {
        debug!("Parsing: program");
        let position = self.current.position;
        let program = match self.current.typ {
            TokenType::Keyword(Keyword::Loop) => self.parse_loop(),
            TokenType::Identifier => self.parse_assignment(),
            _ => return Err(self.err("Invalid program"))
        };

        match self.current.typ {
             TokenType::Semicolon => {
                 self.bump();
                 let p1 = try!(program);
                 let p2 = try!(self.parse_program());
                 Ok(ast::Program {
                     position: position,
                     kind: ast::ProgramKind::Chain(Box::new(p1), Box::new(p2))
                 })
             },
             _ => program
         }
    }

    pub fn parse(&mut self) -> ParsingResult<ast::Program> {
        debug!("Parsing");
        let program = try!(self.parse_program());
        try!(self.expect_type(TokenType::Eof));
        Ok(program)
    }
}
