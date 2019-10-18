use std::error::Error;
use std::fmt;
use crate::patterns;
use crate::tokens::{
    TokenKind,
    Token,
    LiteralKind,
    SeparatorKind,
    OperatorKind,
    KeywordKind,
    SrcCursor
};

/// A Parsing error, should warn the user 
/// about which token were involved
#[derive(Clone)]
pub struct ParsingError {
    msg: String,
}

impl ParsingError {
    fn new(msg: String) -> Self {
        Self { msg: msg }
    }
}
impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg,)
    }
}
impl fmt::Debug for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg,)
    }
}

#[derive(Clone, Debug, PartialEq)]
/// const expression
pub enum Const {
    /// 8 bytes float
    Num(f64),
    /// A boolean
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
/// Numerical operators
pub enum NumericalOp {
    Add,
    Sub,
    Div,
    Mul,
    Pow,
    Mod,
}

#[derive(Clone, Debug, PartialEq)]
/// Comparison operators
pub enum ComparisonOp {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
/// Logical operators
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq)]
/// A unary operation on a single value
pub enum UnaryOp {
    /// `-i`
    Minus,
    /// `+1`
    Plus,
    /// `!i`
    Not,
}

#[derive(Clone, Debug, PartialEq)]
/// A binary operation between 2 values
pub enum BinaryOp {
    /// Numeric operation
    Numerical(NumericalOp),
    /// Comparitive operation
    Comparison(ComparisonOp),
    /// Logical operation
    Logical(LogicalOp),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FlatExp {
    /// Run a operation between 2 expressions
    /// consumes 2 expressions
    FlatBinaryOp(BinaryOp),
    /// Run an operation on a value
    /// consumes 1 expression
    FlatUnaryOp(UnaryOp),
    /// Make a constant value
    /// consumes 1 expression
    FlatConst(Const),
    /// Run several expressions from top-to-bottom
    /// consumes `n` expressions
    FlatBlock(usize),
    /// Call a function with some values
    /// consumes `n` expression => one for each function argument
    FlatCall(String, usize),
    /// Repeatedly run an expression while the conditional expression resolves to true
    /// consumes 2 expressions => the condition, the block
    FlatWhileLoop,
    /// Check if a conditional expression is true and run an expression if it is and another expression if it isn't
    /// consumes 2 or 3 expression => the condition, the 'true' block, the 'false' block
    FlatIf(usize),
    /// Create a function with the given name, arguments, and expression
    /// consumes `n` expressions (with n >= 1) => each argument, the function block
    FlatFunctionDecl(usize),
    /// Return the expression from a function
    /// consumes 1 expression
    FlatReturn,
    /// Assign an expression to a value
    /// consumes 2 expression
    FlatAssign,
    /// Let declaraton
    /// consumes 2 expression
    FlatLetDecl,
    /// a single statement (i.e a line followed by an `;`)
    /// consumes 1 expression
    FlatStatement,
}

pub fn parse_flat_expressions(tokens: &[Token]) -> Result<Vec<FlatExp>, ParsingError> {
    use FlatExp::*;
    let mut exprs: Vec<FlatExp> = Vec::new();
    // in this vector, a None item represent a parsed Token
    let mut unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
    let mut idx: usize = 0;
    while idx < unparsed_tokens.len() {
        // pass parsed tokens
        if unparsed_tokens[idx].is_none() {
            idx +=1;
            continue;
        }

        // match expresions patterns
        let (flat_exp, parsed_tokens) =
            if let Some(exp) = patterns::match_block(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_binary_op(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_unary_op(&unparsed_tokens[idx..]) { exp }
            else { return Err(ParsingError::new("could not parse token".into()));};

        // store parsed expression into a flat tree
        exprs.push(flat_exp);
        
        // consume parsed tokens
        for i in parsed_tokens {
            unparsed_tokens[idx + i] = None;
        }
    }

    Ok(exprs)
}

