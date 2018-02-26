use lexer::Position;

#[derive(Debug, Clone)]
pub struct Program {
    pub position: Position,
    pub kind: ProgramKind
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Nop,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo
}

#[derive(Debug, Clone)]
pub enum ProgramKind {
    // ident1 := ident2 <op> constant
    Assignment(String, String, BinaryOperator, String),

    // loop x1 do p1 end
    Loop(String, Box<Program>),

    // p1;p2
    Chain(Box<Program>, Box<Program>)
}
