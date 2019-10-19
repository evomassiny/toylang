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
pub enum Expr {
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
    LetDecl(String, Box<Expr>),
}

pub struct Ast {
    pub root: Expr,
}
impl Ast {
    
    pub fn from_flat_expressions(flat_exps: &mut Vec<FlatExp>) -> Option<Self> {
        use Expr::*;
        use FlatExp::*;
        let mut stacked_exps: Vec<Expr> = vec![];
        while let Some(flat_exp) = flat_exps.pop() {
            match flat_exp {
                FlatBinaryOp(binary_op) => {
                    let right_hand = stacked_exps.pop()?;
                    let left_hand = stacked_exps.pop()?;
                    stacked_exps.push(
                        BinaryOp(binary_op, Box::new(left_hand), Box::new(right_hand))
                    );
                },
                FlatUnaryOp(unary_op) => {
                    let exp = stacked_exps.pop()?;
                    stacked_exps.push(
                        UnaryOp(unary_op, Box::new(exp))
                    );
                },
                FlatConst(c) => {
                    stacked_exps.push(Const(c));
                },
                FlatBlock(sub_exp_nb) => {
                    let mut sub_exps: Vec<Expr> = vec![];
                    for i in 0..sub_exp_nb {
                        sub_exps.push(stacked_exps.pop()?);
                    }
                    sub_exps.reverse(); // because we are iterating flat_exp in reverse
                    stacked_exps.push(Block(sub_exps));
                },
                FlatCall(name, arg_nb) => {
                    let mut args: Vec<Expr> = vec![];
                    for i in 0..arg_nb {
                        args.push(stacked_exps.pop()?);
                    }
                    args.reverse(); // because we are iterating flat_exp in reverse
                    stacked_exps.push(Call(name, args));
                },
                FlatWhileLoop => {
                    let run_block = Box::new(stacked_exps.pop()?);
                    let cond_exp = Box::new(stacked_exps.pop()?);
                    stacked_exps.push(WhileLoop(cond_exp, run_block));
                },
                FlatLocal(name) => stacked_exps.push(Local(name)),
                FlatIf(sub_exp_count) => {
                    let mut false_block = None;
                    if sub_exp_count == 3 {
                        false_block = Some(Box::new(stacked_exps.pop()?));
                    }
                    let true_block = Box::new(stacked_exps.pop()?);
                    let cond_exp = Box::new(stacked_exps.pop()?);
                    stacked_exps.push(
                        If(cond_exp, true_block, false_block)
                    );
                },
                FlatFunctionDecl(name, args) => {
                    let fn_exp = FunctionDecl(name, args, Box::new(stacked_exps.pop()?));
                    stacked_exps.push(fn_exp);
                },
                FlatReturn(sub_exp_count) => {
                    if sub_exp_count == 1 {
                        let arg = stacked_exps.pop()?;
                        stacked_exps.push(Return(Some(Box::new(arg))));
                    } else {
                        stacked_exps.push(Return(None));
                    }
                },
                FlatLetDecl(id) => {
                    let arg = stacked_exps.pop()?;
                    stacked_exps.push(LetDecl(id, Box::new(arg)));
                },
                FlatFenced => {},
            }
        }
        stacked_exps.reverse();
        Some( Self { root: Block(stacked_exps) })
    }
}

