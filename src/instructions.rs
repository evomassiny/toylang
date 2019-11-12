use crate::ast::{
    //Ast,
    Expr
};
use crate::parser::Const;

#[derive(Debug,Clone,PartialEq)]
pub enum Value {
    /// any quoted string
    Str(String),
    /// int or float
    Num(f64),
    /// boolean `true` or `false`
    Bool(bool),
    /// 
    Function,
    /// 
    Null,
}
impl Value {
    pub fn from_const(expr: &Const) -> Self { 
        return match *expr {
            Const::Str(ref s) => Self::Str(s.clone()),
            Const::Num(n) => Self::Num(n),
            Const::Bool(b) => Self::Bool(b),
        };
    }
}

/// a Code address, i.e: a location in an instruction set
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Address {
    /// a number used to identify any instruction
    Mark(usize),
    /// An actual index in an instruction set
    Index(usize),
}

#[derive(Debug,Clone,PartialEq)]
pub enum Instruction {
    // jump
    Goto(Address),
    GotoIf(Address),
    Label(Address),
    // Variable bindings
    Load,
    Store,
    // value 
    Val(Value),
    // Arguments and return value
    PushStack,
    PopStack,
    FnCall(String, usize),
    FnRet,
    // Native function
    // ... TODO
    // Binary Instructions
    // * Numerical
    Add,
    Sub,
    Div,
    Mul,
    Pow,
    Mod,
    // * Comparison
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    // * Logical
    And,
    Or,
    // Unary Instructions
    Minus,
    Plus,
    Not,
}

