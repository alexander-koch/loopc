//! Abstract syntax tree definition
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

use lexer::Position;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub position: Position,
    pub kind: ProgramKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Nop,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgramKind {
    // ident1 := ident2 <op> constant
    Assignment(String, String, BinaryOperator, String),

    // loop x1 do p1 end
    Loop(String, Box<Program>),

    // p1;p2
    Chain(Box<Program>, Box<Program>),
}
