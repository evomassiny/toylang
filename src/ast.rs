use crate::parser::{
    FlatExp,
    FlatExp::*,
    Const,
    UnaryOp,
    BinaryOp,
    BinaryOp::*,
    ComparisonOp,
    LogicalOp,
    NumericalOp,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    /// Run a operation between 2 expressions
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    /// Run an operation on a value
    UnaryOp(UnaryOp, Box<Expr>),
    /// Make a constant value
    Const(Const),
    /// Run several expressions from top-to-bottom
    Block(Vec<Expr>),
    /// Call a function with some values
	/// each sub expressions being arguments
    Call(String, Vec<Expr>),
    /// Repeatedly run an expression while 
    /// the conditional expression resolves to true
    /// consumes 2 expressions => the condition, the block
    WhileLoop(Box<Expr>, Box<Expr>),
    /// Load a value from a reference (eg a variable name)
    Local(String),
    /// An If expression with its condition, 
    /// the block expressed when the condition holds true,
    /// and optionaly the block expressed when it doesn't
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    /// Create a function with the given name, arguments, and expression
    FunctionDecl(String, Vec<String>, Box<Expr>),
    /// Return the expression from a function
    Return(Option<Box<Expr>>),
    /// Let declaraton
    LetDecl(String),
}

pub struct Ast {
    pub root: Exp,
}
impl Ast {
    
    pub fn from_flat_expressions(flat_exprs: Vec<FlatExp>) -> Self {
        let mut stacked_exprs: Vec<Expr> = vec![];

    }
}

