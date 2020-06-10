//! Syntax analysis.
//! Generates an abstract syntax tree based on a vector of tokens.
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

use std::iter::Peekable;
use lexer::{Error, Keyword, Position, Token, TokenType};

pub mod ast;
pub type ParsingResult<T> = ::std::result::Result<T, Error>;

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    current: Token,
    strict: bool,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Creates a new parser based on an iterator of tokens.
    pub fn new(it: T, strict: bool) -> Parser<T> {
        let mut peek = it.peekable();
        let start = peek.next().unwrap_or(Token {
            typ: TokenType::Eof,
            value: None,
            position: Position::new(-1, -1),
        });
        Parser {
            tokens: peek,
            current: start,
            strict: strict,
        }
    }

    /// Generates an error object.
    ///
    /// # Arguments
    /// * `message` - Error message do display
    fn err(&self, message: String) -> Error {
        Error {
            message: message,
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
            Err(self.err(format!(
                "Unexpected token `{:?}`, expected: `{:?}`",
                self.current.typ, t
            )))
        }
    }

    fn expect_one_of_types(&mut self, tv: Vec<TokenType>) -> ParsingResult<()> {
        if tv.contains(&self.current.typ) {
            self.bump();
            Ok(())
        } else {
            Err(self.err(format!(
                "Unexpected token `{:?}`, expected one of: `{:?}`",
                self.current.typ, tv
            )))
        }
    }

    pub fn parse_loop(&mut self) -> ParsingResult<ast::Program> {
        debug!("Parsing: loop");
        let position = self.current.position;
        self.expect_type(TokenType::Keyword(Keyword::Loop))?;

        let ident = self.get_current_value();
        self.expect_type(TokenType::Identifier)?;
        self.expect_type(TokenType::Keyword(Keyword::Do))?;
        let program = self.parse_program()?;
        self.expect_type(TokenType::Keyword(Keyword::End))?;

        Ok(ast::Program {
            position: position,
            kind: ast::ProgramKind::Loop(ident, Box::new(program)),
        })
    }

    pub fn parse_assignment(&mut self) -> ParsingResult<ast::Program> {
        debug!("Parsing: assignment");
        let position = self.current.position;
        let assignee = self.get_current_value();
        self.expect_type(TokenType::Identifier)?;
        self.expect_type(TokenType::Assignment)?;

        let lhs = self.get_current_value();
        if self.strict {
            self.expect_type(TokenType::Identifier)?;
        } else {
            self.expect_one_of_types(vec![TokenType::Identifier, TokenType::Constant])?;
        }

        let operator = match self.current.typ {
            TokenType::Plus => ast::BinaryOperator::Plus,
            TokenType::Minus => ast::BinaryOperator::Minus,
            TokenType::Multiply => {
                if self.strict {
                    return Err(self.err(format!(
                        "Built-in multiplication is not allowed in strict mode!"
                    )));
                } else {
                    ast::BinaryOperator::Multiply
                }
            }
            TokenType::Keyword(Keyword::Div) => {
                if self.strict {
                    return Err(self.err(format!(
                        "Built-in division is not allowed in strict mode!"
                    )));
                } else {
                    ast::BinaryOperator::Divide
                }
            }
            TokenType::Keyword(Keyword::Mod) => {
                if self.strict {
                    return Err(self.err(format!("Built-in modulo is not allowed in strict mode!")));
                } else {
                    ast::BinaryOperator::Modulo
                }
            }
            _ => {
                if self.strict {
                    return Err(self.err(format!(
                        "Operator is missing, found '{:?}' instead",
                        self.current.typ
                    )));
                } else {
                    return Ok(ast::Program {
                        position: position,
                        kind: ast::ProgramKind::Assignment(
                            assignee,
                            lhs,
                            ast::BinaryOperator::Nop,
                            "".to_owned(),
                        ),
                    });
                }
            }
        };
        self.bump();

        let rhs = self.get_current_value();
        if self.strict {
            self.expect_type(TokenType::Constant)?;
        } else {
            self.expect_one_of_types(vec![TokenType::Identifier, TokenType::Constant])?;
        }

        Ok(ast::Program {
            position: position,
            kind: ast::ProgramKind::Assignment(assignee, lhs, operator, rhs),
        })
    }

    pub fn parse_program(&mut self) -> ParsingResult<ast::Program> {
        debug!("Parsing: program");
        let position = self.current.position;
        let program = match self.current.typ {
            TokenType::Keyword(Keyword::Loop) => self.parse_loop(),
            TokenType::Identifier => self.parse_assignment(),
            _ => return Err(self.err("Invalid program".into())),
        };

        match self.current.typ {
            TokenType::Semicolon => {
                self.bump();
                let p1 = program?;
                let p2 = self.parse_program()?;
                Ok(ast::Program {
                    position: position,
                    kind: ast::ProgramKind::Chain(Box::new(p1), Box::new(p2)),
                })
            }
            _ => program,
        }
    }

    pub fn parse(&mut self) -> ParsingResult<ast::Program> {
        debug!("Parsing");
        let program = self.parse_program()?;
        self.expect_type(TokenType::Eof)?;
        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;

    fn evaluate(input: &str, strict: bool) -> ast::Program {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.run();
        assert!(tokens.is_ok());

        let mut parser = Parser::new(tokens.unwrap().into_iter(), strict);
        let ast = parser.parse();
        assert!(ast.is_ok());
        ast.unwrap()
    }

    #[test]
    fn test_double() {
        let input = "x0 := x1 + 5;\nloop x0 do\n\tx0 := x0 + 1\nend\n";
        let ast = evaluate(input, true);

        let p1 = ast::Program {
            position: Position::new(1, 1),
            kind: ast::ProgramKind::Assignment(
                "x0".into(),
                "x1".into(),
                ast::BinaryOperator::Plus,
                "5".into(),
            ),
        };

        let sub = ast::Program {
            position: Position::new(3, 2),
            kind: ast::ProgramKind::Assignment(
                "x0".into(),
                "x0".into(),
                ast::BinaryOperator::Plus,
                "1".into(),
            ),
        };

        let p2 = ast::Program {
            position: Position::new(2, 1),
            kind: ast::ProgramKind::Loop("x0".into(), Box::new(sub)),
        };

        assert_eq!(
            ast,
            ast::Program {
                position: Position::new(1, 1),
                kind: ast::ProgramKind::Chain(Box::new(p1), Box::new(p2)),
            }
        );
    }
}
