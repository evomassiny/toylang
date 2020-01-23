//!
//! This Module gathers everything from the lexing of the input script
//! to the actual Abstract Syntax Tree builder itself.
//! 
//! An AST is build as follow:
//! * first we read the input javascript file and we *lex* it into a vector of *tokens*
//! * then we build a flat represention (in Reverse Polish Notation) of the AST by matching the token vector 
//! against known expressions patterns
//! * Finally we construct an actual AST from its flat representation.
//! 
pub mod lexer;
pub mod tokens;
pub mod patterns;
pub mod parser;
pub mod ast;

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

