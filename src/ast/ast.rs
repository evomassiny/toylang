use std::error::Error;
use std::str::FromStr;
use crate::ast::lexer::lex;
use crate::ast::parser::{
    FlatExp,
    Literal,
    UnaryOp,
    BinaryOp,
    parse_flat_expressions,
    ParsingError,
};
use crate::ast::tokens::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// Run a operation between 2 expressions
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    /// Run an operation on a value
    UnaryOp(UnaryOp, Box<Expr>),
    /// Make a constant value
    Literal(Literal),
    /// Run several expressions from top-to-bottom
    Block(Vec<Expr>),
    /// Call on an expression
	/// each sub expressions being arguments
    Call(Box<Expr>, Vec<Expr>),
    /// Repeatedly run an expression while 
    /// the conditional expression resolves to true
    /// consumes 2 expressions => the condition, the block
    WhileLoop(Box<Expr>, Box<Expr>),
    /// Repeatedly run an expression while 
    /// the conditional expression resolves to true
    /// consumes 4 expressions:
    /// * the initializer,
    /// * the condition,
    /// * the increment,
    /// * the block
    ForLoop(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>),
    /// Load a value from a reference (eg a variable name)
    Local(String),
    /// Store a value into a reference (eg a variable name)
    Assign(String, Box<Expr>),
    /// An If expression with its condition, 
    /// the block expressed when the condition holds true,
    /// and optionaly the block expressed when it doesn't
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    /// Create a function with the given name, arguments, and expression
    FunctionDecl(String, Vec<String>, Box<Expr>),
    /// Return the expression from a function
    Return(Option<Box<Expr>>),
    /// quit the current
    Break,
    /// quit the current loop evaluation and go to the next one
    Continue,
    /// Let declaraton
    LetDecl(String, Option<Box<Expr>>),
}
impl Expr {
    /// returns the number of sub-expressions that must be evaluated before `self`
    pub fn count_sub_expressions(&self) -> usize {
        use Expr::*;
        match self {
            BinaryOp(..) => 2,
            UnaryOp(..) => 1,
            Literal(_) => 0,
            Block(exprs) => exprs.len(),
            Call(_id, exprs) => exprs.len() + 1,
            WhileLoop(..) => 2,
            ForLoop(..) => 4,
            Local(_) => 0,
            Assign(..) => 1,
            If(_cond, _true_block, false_block) => {
                if false_block.is_some() { 3 } else { 2 }
            } ,
            FunctionDecl(..) => 1,
            Return(e) => if e.is_some() { 1 } else { 0 },
            LetDecl(_, e) => if e.is_some() { 1 } else { 0 },
            Break | Continue => 0,
        }
    }

    /// returns either or this expression should return a value.
    pub fn has_return_value(&self) -> bool {
        use Expr::*;
        match self {
            BinaryOp(..) => true,
            UnaryOp(..) => true,
            Literal(..) => true,
            Block(..) => false,
            Call(..) => true,
            WhileLoop(..) => false,
            ForLoop(..) => false,
            Local(..) => false,
            Assign(..) => true,
            If(..) => false,
            FunctionDecl(..) => true,
            Return(..) => true,
            LetDecl(..) => false,
            Break | Continue => false,
        }
    }

    /// returns the references to the sub-expressions 
    /// that must be evaluated before `self`
    pub fn get_sub_expression(&self, idx: usize) -> Option<&Expr> {
        use Expr::*;
        match self {
            BinaryOp(_op, a, b) => {
                match idx {
                    0 => Some(a.as_ref()),
                    1 => Some(b.as_ref()),
                    _ => None
                }
            },
            UnaryOp(_op, a) => {
                match idx {
                    0 => Some(a.as_ref()),
                    _ => None
                }
            },
            Literal(_) => None,
            Block(exprs) => Some(exprs.get(idx)?),
            Assign(_name, e) => {
                match idx {
                    0 => Some(e.as_ref()),
                    _ => None,
                }
            },
            Call(id, args) => {
                match idx {
                    0 => Some(id.as_ref()),
                    i => Some(args.get(i -1)?),
                }
            },
            WhileLoop(cond, block) => {
                match idx {
                    0 => Some(cond.as_ref()),
                    1 => Some(block.as_ref()),
                    _ => None
                }
            },
            ForLoop(init, cond, inc, block) => {
                match idx {
                    0 => Some(init.as_ref()),
                    1 => Some(cond.as_ref()),
                    2 => Some(inc.as_ref()),
                    3 => Some(block.as_ref()),
                    _ => None
                }
            },
            Local(_) => None,
            If(cond, true_block, ref false_block) => {
                match idx {
                    0 => Some(cond.as_ref()),
                    1 => Some(true_block.as_ref()),
                    2 => match false_block {
                        Some(e) => Some(e.as_ref()),
                        None => None,
                    },
                    _ => None
                }
            } ,
            FunctionDecl(_name, _args, block) => {
                match idx {
                    0 => Some(block.as_ref()),
                    _ => None
                }
            },
            Return(e) => {
                match idx {
                    0 => match e {
                        Some(e) => Some(e.as_ref()),
                        None => None,
                    },
                    _ => None
                }
            },
            LetDecl(_name, e) => {
                match idx {
                    0 => match e {
                        Some(e) => Some(e.as_ref()),
                        None => None,
                    },
                    _ => None
                }
            },
            Break | Continue => None,
        }
    }
}

#[derive(Debug)]
pub struct Ast {
    pub root: Expr,
}
impl Ast {
    
    /// Consumes each FlatExp of `flat_exps` to build an Abstract Syntax Tree from it.
    pub fn from_flat_expressions(flat_exps: &mut Vec<FlatExp>) -> Option<Self> {
        use Expr::*;
        use FlatExp::*;
        let mut exp_stack: Vec<Expr> = vec![];
        while let Some(flat_exp) = flat_exps.pop() {
            match flat_exp {
                // pop the 2 operands and push a BinaryOp expr
                FlatBinaryOp(binary_op) => {
                    let left_hand = exp_stack.pop()?;
                    let right_hand = exp_stack.pop()?;
                    exp_stack.push(
                        BinaryOp(binary_op, Box::new(left_hand), Box::new(right_hand))
                    );
                },
                // pop the only operand and push a BinaryOp expr
                FlatUnaryOp(unary_op) => {
                    let exp = exp_stack.pop()?;
                    exp_stack.push(
                        UnaryOp(unary_op, Box::new(exp))
                    );
                },
                // pop the right hand of the assignation
                FlatAssign(name) => {
                    let exp = exp_stack.pop()?;
                    exp_stack.push(
                        Assign(name, Box::new(exp))
                    );
                },
                // push the Literal
                FlatLiteral(c) => {
                    exp_stack.push(Literal(c));
                },
                // Collect each subexpressions and push a Block
                FlatBlock(sub_exp_nb) => {
                    let mut sub_exps: Vec<Expr> = vec![];
                    for _ in 0..sub_exp_nb {
                        sub_exps.push(exp_stack.pop()?);
                    }
                    exp_stack.push(Block(sub_exps));
                },
                // Collect each argument, and push a Call
                FlatCall(arg_nb) => {
                    let id_exp = Box::new(exp_stack.pop()?);
                    let mut args: Vec<Expr> = vec![];
                    for _ in 0..(arg_nb -1) {
                        args.push(exp_stack.pop()?);
                    }
                    exp_stack.push(Call(id_exp, args));
                },
                // Collect the condition expression and the loop block,
                // then push a WhileLoop
                FlatWhileLoop => {
                    let cond_exp = Box::new(exp_stack.pop()?);
                    let run_block = Box::new(exp_stack.pop()?);
                    exp_stack.push(WhileLoop(cond_exp, run_block));
                },
                // Collect the 
                // * init expression,
                // * the condition expression,
                // * increment expression
                // * the loop block,
                // then push a ForLoop
                FlatForLoop => {
                    let init_exp = Box::new(exp_stack.pop()?);
                    let cond_exp = Box::new(exp_stack.pop()?);
                    let inc_exp = Box::new(exp_stack.pop()?);
                    let run_block = Box::new(exp_stack.pop()?);
                    exp_stack.push(ForLoop(init_exp, cond_exp, inc_exp, run_block));
                },
                // push a Local
                FlatLocal(name) => exp_stack.push(Local(name)),
                // Collect the condition expression and the true block,
                // and the false block (if any)
                // then push an If
                FlatIf(sub_exp_count) => {
                    let cond_exp = Box::new(exp_stack.pop()?);
                    let true_block = Box::new(exp_stack.pop()?);
                    let mut false_block = None;
                    if sub_exp_count == 3 {
                        false_block = Some(Box::new(exp_stack.pop()?));
                    }
                    exp_stack.push(
                        If(cond_exp, true_block, false_block)
                    );
                },
                // Collect the arguments, and the function block
                // then push a FunctionDecl
                FlatFunctionDecl(name, args) => {
                    let fn_exp = FunctionDecl(name, args, Box::new(exp_stack.pop()?));
                    exp_stack.push(fn_exp);
                },
                // Collect the returned expression (if any) 
                // then push a Return
                FlatReturn(sub_exp_count) => {
                    if sub_exp_count == 1 {
                        let arg = exp_stack.pop()?;
                        exp_stack.push(Return(Some(Box::new(arg))));
                    } else {
                        exp_stack.push(Return(None));
                    }
                },
                // Collect the expression we are assigning to `id` (if any)
                // then push a LetDecl
                FlatLetDecl(id, sub_exp_count) => {
                    if sub_exp_count == 1 {
                        let arg = exp_stack.pop()?;
                        exp_stack.push(LetDecl(id, Some(Box::new(arg))));
                    } else {
                        exp_stack.push(LetDecl(id, None));
                    }
                },
                FlatBreak => exp_stack.push(Break),
                FlatContinue => exp_stack.push(Continue),
                // those aren't expression, but bounds
                FlatFenced => {},
            }
        }
        // collect all expression trees into one root
        exp_stack.reverse();
        let root: Expr = Block(exp_stack);
        Some(Self { root })
    }


    /// build an Abstract Syntax Tree from a Token slice
    pub fn from_tokens(tokens : &[Token]) -> Result<Self, Box<dyn Error>> {
        let mut flat_exps = parse_flat_expressions(tokens)?;
        Self::from_flat_expressions(&mut flat_exps).ok_or(Box::new(
                ParsingError::new("Failed to parse expression into an AST.".into())
            )
        )
    }
}
    
impl FromStr for Ast {
    type Err = Box<dyn Error>;

    /// lex, parse source code into an Abstract Syntax Tree
    fn from_str(src: &str) -> Result<Self, Self::Err> {
        let tokens = lex(src)?;
        Self::from_tokens(&tokens)
    }
}

