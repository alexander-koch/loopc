use lexer::Position;

#[derive(Debug, Clone)]
pub struct Program {
    pub position: Position,
    pub kind: ProgramKind
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus
}

#[derive(Debug, Clone)]
pub enum ProgramKind {
    // loop x1 do p1 end
    Loop(String, Box<Program>),

    // ident1 := ident2 <op> constant
    Assignment(String, String, BinaryOperator, String),

    // p1;p2
    Chain(Box<Program>, Box<Program>)
}
