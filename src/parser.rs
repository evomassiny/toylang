use std::error::Error;
use std::fmt;
use crate::patterns;
use crate::tokens::{
    TokenKind,
    Token,
    SrcCursor
};

/// A Parsing error, should warn the user 
/// about which token were involved
#[derive(Clone, PartialEq)]
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
    /// A string
    Str(String),
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
    /// Assignation
    Assign,
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
    /// Load a value from a reference (eg a variable name)
    FlatLocal(String),
    /// Check if a conditional expression is true and run an expression if it is and another expression if it isn't
    /// consumes 2 or 3 expression => the condition, the 'true' block, the 'false' block
    FlatIf(usize),
    /// Create a function with the given name, arguments, and expression
    /// consumes 1 expression (its block)
    FlatFunctionDecl(String, Vec<String>),
    /// Return the expression from a function
    /// consumes 0 or 1 expression, depending on either or not it returns something
    FlatReturn(usize),
    /// Let declaraton
    /// consumes 1 expression
    FlatLetDecl(String),
    /// a fenced statement (i.e a line followed by an `;`, or a statement enclosed in parenthesis)
    /// does not consume any sub-expressions
    FlatFenced,
}

/// Parse a token slice into a flat representation 
/// of an Abstract Syntax Tree, in Polish Notation
pub fn parse_flat_expressions(tokens: &[Token]) -> Result<Vec<FlatExp>, ParsingError> {
    let mut exprs: Vec<FlatExp> = Vec::new();
    // in this vector, a None item represent a parsed Token
    let mut unparsed_tokens: Vec<Option<&Token>> = tokens.iter()
        .filter(|t|  // weed out comments
            match t { 
                &Token { kind: TokenKind::Comment(_), .. } => false,
                _ => true,
            })
        .map(|t| Some(t)) // as Option<&Token>
        .collect();
    let mut idx: usize = 0;
    while idx < unparsed_tokens.len() {
        // pass parsed tokens
        if unparsed_tokens[idx].is_none() {
            idx +=1;
            continue;
        }

        // match expresions patterns, order matters.
        let (flat_exp, parsed_tokens) =
                 if let Some(exp) = patterns::match_block(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_function_declaration(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_let(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_if(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_while(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_return(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_function_call(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_semi_colomn_fenced(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_binary_op(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_parenthesis_fenced(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_unary_op(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_const(&unparsed_tokens[idx..]) { exp }
            else if let Some(exp) = patterns::match_local(&unparsed_tokens[idx..]) { exp }
            else { 
                match &unparsed_tokens[idx] {
                    Some(Token { cursor, .. }) => {
                        return Err(ParsingError::new(
                            format!(
                                "could not parse expression at line {}, column {}", 
                                cursor.line + 1,
                                cursor.column + 1,
                            )
                        ));
                    },
                    None => return Err(ParsingError::new("unexpected end".into())),
               }
            };

        // store parsed expression into a flat tree
        match flat_exp {
            FlatExp::FlatFenced => {}, // ignore thoses, they are just separators between expressions
            _ => exprs.push(flat_exp),
        }
        // consume parsed tokens
        for i in parsed_tokens {
            unparsed_tokens[idx + i] = None;
        }
    }
    Ok(exprs)
}


mod test {
    use crate::lexer::lex;
    use crate::parser::{
        FlatExp,
        Const,
        UnaryOp,
        BinaryOp,
        BinaryOp::*,
        ComparisonOp,
        NumericalOp,
        LogicalOp,
        parse_flat_expressions,
    };

    #[test]
    fn test_parse_flat_expressions() {
        use FlatExp::*;
        let tokens = lex("{ let A = 1; }").unwrap();
        assert_eq!(
            parse_flat_expressions(&tokens),
            Ok(vec![
               FlatBlock(1),
               FlatLetDecl("A".into()),
               FlatConst(Const::Num(1.)),
           ])
        );

        let tokens = lex(r#"
        fn foo(a, b) {
             let c = a + b; 
             return c;
         }
         foo(1, 3);
        "#).unwrap();
        assert_eq!(
            parse_flat_expressions(&tokens),
            Ok(vec![
               FlatFunctionDecl(
                   "foo".into(),
                   vec!["a".into(), "b".into()],
               ),
               FlatBlock(2),
               FlatLetDecl("c".into()),
               FlatBinaryOp(Numerical(NumericalOp::Add)),
               FlatLocal("a".into()),
               FlatLocal("b".into()),
               FlatReturn(1),
               FlatLocal("c".into()),
               FlatCall("foo".into(), 2),
               FlatConst(Const::Num(1.)),
               FlatConst(Const::Num(3.)),
           ])
        );
    }
}
