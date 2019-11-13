use crate::ast::Expr;
use crate::instructions::{Value,Instruction,Address};
use crate::instructions_set::InstructionSet;

pub struct Executor<'inst> {
    pub instructions: &'inst InstructionSet,
    pub value_stacks: Vec<Vec<Value>>,
    pub passthrough: Vec<Value>,
    pub execution_pointers: Vec<usize>
}
