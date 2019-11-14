use crate::ast::Expr;
use crate::instructions::{Value,Instruction};
use crate::compiler::Compiler;

pub struct Executor<'inst> {
    pub instructions: &'inst Vec<Instruction>,
    pub value_stacks: Vec<Vec<Value>>,
    pub passthrough: Vec<Value>,
    pub execution_pointers: Vec<usize>
}
