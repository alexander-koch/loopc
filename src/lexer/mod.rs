//! Lexical analysis.
//! Generates tokens based on an input file.
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

use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Keyword {
    Loop,
    Do,
    End,
    Div,
    Mod,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TokenType {
    Eof,
    Space,
    Identifier,
    Constant,
    Plus,
    Minus,
    Multiply,
    Assignment,
    Semicolon,
    Keyword(Keyword),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct Position {
    pub line: i32,
    pub column: i32,
}

impl Position {
    pub fn new(line: i32, column: i32) -> Position {
        Position {
            line: line,
            column: column,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Error {
    pub message: String,
    pub position: Position,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.message)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub value: Option<String>,
    pub position: Position,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token ({:?}, {:?})", self.typ, self.value)
    }
}

pub type LexicalResult<T> = ::std::result::Result<T, Error>;

pub struct Lexer<'a> {
    data: &'a str,
    cursor: usize,
    current: Option<char>,
    position: Position,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer structure
    ///
    /// # Arguments
    ///
    /// * `src` - The source code string.
    ///
    pub fn new(src: &str) -> Lexer {
        let first = src.chars().next();
        Lexer {
            data: src,
            cursor: 0,
            current: first,
            position: Position::new(1, 1),
        }
    }

    /// Consume a character
    fn consume(&mut self) {
        self.cursor += self.current.unwrap().len_utf8();
        if self.cursor < self.data.len() {
            let ch = get_char(&self.data, self.cursor);
            self.position.column += 1;
            self.current = Some(ch);
        } else {
            self.current = None;
        }
    }

    /// Test if the current character is equal to c
    fn curr_is(&self, c: char) -> bool {
        self.current == Some(c)
    }

    /// Peek in front of the buffer, at a certain offset
    fn peek(&mut self, offset: usize) -> Option<char> {
        let incr = self.current.unwrap().len_utf8() * offset;
        if self.cursor + incr < self.data.len() {
            Some(get_char(&self.data, self.cursor + incr))
        } else {
            None
        }
    }

    fn err(&self, msg: &str, position: Position) -> Error {
        Error {
            message: msg.into(),
            position: position,
        }
    }

    fn is_newline(&mut self) -> bool {
        if self.current == Some('\n') {
            true
        } else if self.current == Some('\r') && self.peek(1) == Some('\n') {
            true
        } else {
            false
        }
    }

    fn skip_space(&mut self) {
        if self.current == Some('\r') && self.peek(1) == Some('\n') {
            self.consume();
            self.consume();
            self.position.line += 1;
            self.position.column = 1;
        } else if self.current == Some('\r') {
            self.consume();
            self.position.column = 1;
        } else if self.current == Some('\n') {
            self.consume();
            self.position.line += 1;
            self.position.column = 1;
        } else {
            self.consume();
        }
    }

    /// Tests if the current character is a whitespace
    fn is_space(&mut self) -> bool {
        self.current.map(|x| x.is_whitespace()).unwrap_or(false)
    }

    fn is_constant(&mut self) -> bool {
        self.current.map(|v| v.is_digit(10)).unwrap_or(false)
    }

    fn is_punctuation(&mut self) -> bool {
        self.current
            .map(|c| match c {
                '+' | '-' | '*' | ':' | ';' | '/' => true,
                _ => false,
            })
            .unwrap_or(false)
    }

    fn is_identifier_beginning(&mut self) -> bool {
        match self.current {
            Some(c) => c.is_alphabetic(),
            None => false,
        }
    }

    fn is_identifier(&mut self) -> bool {
        match self.current {
            Some(c) => c.is_alphanumeric() || c == '_',
            None => false,
        }
    }

    fn scan_comment(&mut self) -> LexicalResult<Token> {
        if self.is_space() {
            self.consume();
        }

        let position = self.position;
        let start = self.cursor;
        while let Some(c) = self.current {
            if c == '\n' || c == '\r' {
                break;
            } else {
                self.consume();
            }
        }

        let s = (&self.data[start..self.cursor]).to_string();
        if self.is_newline() {
            self.skip_space();
        }

        Ok(Token {
            typ: TokenType::Space,
            value: Some(s),
            position: position,
        })
    }

    fn scan_constant(&mut self) -> LexicalResult<Token> {
        let start = self.cursor;
        let position = self.position;

        while let Some(c) = self.current {
            if c.is_digit(10) {
                self.consume();
            } else {
                break;
            }
        }

        let s = (&self.data[start..self.cursor]).to_string();
        Ok(Token {
            typ: TokenType::Constant,
            value: Some(s),
            position: position,
        })
    }

    fn scan_punctuation(&mut self) -> LexicalResult<Token> {
        let position = self.position;
        let c = match self.current {
            Some(v) => v,
            None => return Err(self.err("Reached end of file", position)),
        };
        self.consume();

        let kind = match c {
            ';' => TokenType::Semicolon,
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '*' => TokenType::Multiply,
            '/' => {
                if self.curr_is('/') {
                    self.consume();
                    return self.scan_comment();
                } else {
                    return Err(self.err("Invalid token", position));
                }
            }
            ':' => {
                if self.curr_is('=') {
                    self.consume();
                    TokenType::Assignment
                } else {
                    return Err(self.err("Invalid token", position));
                }
            }
            _ => return Err(self.err("Unknown punctuation", position)),
        };

        Ok(Token {
            typ: kind,
            value: None,
            position: position,
        })
    }

    fn scan_identifier(&mut self) -> LexicalResult<Token> {
        let start = self.cursor;
        let position = self.position;
        while self.is_identifier() {
            self.consume();
        }

        // Test for builtin-types
        let s = &self.data[start..self.cursor];
        let kind = match s {
            "loop" => TokenType::Keyword(Keyword::Loop),
            "do" => TokenType::Keyword(Keyword::Do),
            "end" => TokenType::Keyword(Keyword::End),
            "div" => TokenType::Keyword(Keyword::Div),
            "mod" => TokenType::Keyword(Keyword::Mod),
            _ => TokenType::Identifier,
        };

        Ok(Token {
            typ: kind,
            value: if kind == TokenType::Identifier {
                Some(s.to_owned())
            } else {
                None
            },
            position: position,
        })
    }

    fn next_token(&mut self) -> LexicalResult<Token> {
        let position = self.position;
        if self.is_space() || self.is_newline() {
            self.skip_space();
            Ok(Token {
                typ: TokenType::Space,
                value: None,
                position: position,
            })
        } else if self.is_constant() {
            self.scan_constant()
        } else if self.is_punctuation() {
            self.scan_punctuation()
        } else if self.is_identifier_beginning() {
            self.scan_identifier()
        } else {
            Err(self.err("Unknown token type", position))
        }
    }

    /// Tokenizes the source code into a vector of tokens
    pub fn run(&mut self) -> LexicalResult<Vec<Token>> {
        let mut tokens = Vec::new();

        // Read all the tokens
        while self.cursor < self.data.len() {
            let token = try!(self.next_token());
            tokens.push(token);
        }

        tokens.retain(|i| i.typ != TokenType::Space);
        Ok(tokens)
    }
}

/// Helper functions to get a character at a certain position
fn get_char(s: &str, byte: usize) -> char {
    s[byte..].chars().next().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn evaluate(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.run();
        assert!(tokens.is_ok());
        tokens.unwrap()
    }

    #[test]
    fn test_keywords() {
        let input = "loop do end";
        let tokens = evaluate(input);
        assert_eq!(
            tokens,
            vec![
                Token {
                    typ: TokenType::Keyword(Keyword::Loop),
                    value: None,
                    position: Position::new(1, 1),
                },
                Token {
                    typ: TokenType::Keyword(Keyword::Do),
                    value: None,
                    position: Position::new(1, 6),
                },
                Token {
                    typ: TokenType::Keyword(Keyword::End),
                    value: None,
                    position: Position::new(1, 9),
                },
            ]
        )
    }
}
