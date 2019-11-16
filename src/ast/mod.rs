//!
//! This Module gathers everything from the lexing of the input script
//! to the actual Abstract Syntax Tree builder itself.
mod lexer;
mod tokens;
mod patterns;
mod parser;
mod ast;

pub use ast::{
    Ast,
    Expr
};
pub use parser::{
    Const,
    BinaryOp,
    NumericalOp,
    ComparisonOp,
    LogicalOp,
    UnaryOp,
};

