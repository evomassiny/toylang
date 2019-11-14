use std::collections::HashMap;
use crate::ast::Expr;
use crate::preprocessors::{PreValue,PreInstruction};
use crate::preprocessors as pp;
use crate::instructions::{Instruction,Value};


#[derive(Debug)]
pub struct Compiler {}

impl Compiler {

    /// compile an AST into a vector of PreInstruction
    /// an intermediate representation of the actual Instruction set
    /// with unsolved address (labels)
    pub fn preprocess(expression: &Expr) -> Option<Vec<PreInstruction>> {
        use PreInstruction::*;
        let mut instructions: Vec<Vec<PreInstruction>> = Vec::new();
        // traverse the AST
        // and build an instructions list
        // executable by a simple stack machine
        let mut nodes: Vec<&Expr> = Vec::new();
        let mut dep_count: Vec<usize> = Vec::new();

        // a usize used to build uniq labels
        let mut label_counter: usize = 0;

        dep_count.push(0);
        nodes.push(expression);
        // traverse the tree from right to left
        'tree_traversal: while nodes.len() > 0 {
            let idx = nodes.len() -1;
            let node = &nodes[idx];

            // add node dependancies
            let sub_expr_count = node.count_sub_expressions();
            if dep_count[idx] < sub_expr_count {
                dep_count.push(0);
                // assert that we tarverse the tree from right to left
                let next_idx = sub_expr_count - dep_count[idx] - 1;
                if let Some(sub_node) = node.get_sub_expression(next_idx) {
                    nodes.push(&sub_node);
                }
                dep_count[idx] += 1;
                continue 'tree_traversal;
            }

            // at this point, all sub-expressions were already been solved
            // put them in a dedicated Vector, in the correct order
            let mut sub_instructions = Vec::new();
            for i in 0..sub_expr_count {
                sub_instructions.push(instructions.pop()?);
            }
            // Build the intruction set needed to evaluate the node expression
            use Expr::*;
            let insts: Vec<PreInstruction> = match *node {
                Const(c) => pp::preprocess_const(c)?,
                Return(_) => pp::preprocess_return(sub_instructions)?,
                If(..) => pp::preprocess_if(sub_instructions, &mut label_counter)?,
                Block(..) => pp::preprocess_block(sub_instructions)?,
                LetDecl(name, _) => pp::preprocess_let(name, sub_instructions)?,
                Local(id) => pp::preprocess_local(id)?,
                Assign(id, ..) => pp::assign_to_instructions(id, sub_instructions)?,
                Call(..) => pp::preprocess_call(sub_instructions)?,
                FunctionDecl(name, args, _block) => 
                    pp::preprocess_function_decl(sub_instructions, name, args, &mut label_counter)?,
                BinaryOp(op, ..) => 
                    pp::preprocess_binary_op(op, sub_instructions, &mut label_counter)?,
                UnaryOp(op, ..) => pp::preprocess_unary_op(op, sub_instructions)?,
                WhileLoop(..) => pp::preprocess_while(sub_instructions, &mut label_counter)?,
            };
            if insts.len() > 0 {
                instructions.push(insts);
            }

            // pop the current node, it's been solved.
            let _ = nodes.pop();
            let _ = dep_count.pop();
            
        }
        Some(instructions.pop()?)
    }

    /// Compile an AST into an instruction set
    pub fn compile(ast: &Expr) -> Option<Vec<Instruction>> {
        use Instruction::*;
        // compile into proto instructions
        let pre_instructions = Self::preprocess(ast)?;

        // solve labels as instruction offset
        let mut label_to_offset: HashMap<usize, usize> = HashMap::new();
        let mut label_count: usize = 0;
        // collect labels
        for (i, pre_intruction) in pre_instructions.iter().enumerate() {
            if let PreInstruction::AddrLabel(label) = *pre_intruction {
                // store the instruction index 
                label_to_offset.insert(label, i - label_count);
                label_count += 1;
            }
        }

        // build the actual instructions
        let mut instructions: Vec<Instruction> = Vec::new();
        for pre_instruction in pre_instructions {
            match pre_instruction {
                PreInstruction::Goto(label) => {
                    let offset = label_to_offset.get(&label)?;
                    instructions.push(Goto(*offset));
                },
                PreInstruction::GotoIf(label) => {
                    let offset = label_to_offset.get(&label)?;
                    instructions.push(GotoIf(*offset));
                },
                PreInstruction::AddrLabel(_) => {;},
                PreInstruction::NewRef(s) => instructions.push(NewRef(s)),
                PreInstruction::Ref(s) => instructions.push(Ref(s)),
                PreInstruction::Load(s) => instructions.push(Load(s)),
                PreInstruction::Store(s) => instructions.push(Store(s)),
                PreInstruction::Val(pre_val) => {
                    use Value::*;
                    let val = match pre_val {
                        PreValue::Str(s) => Str(s),
                        PreValue::Bool(b) => Bool(b),
                        PreValue::Num(n) => Num(n),
                        PreValue::Function(label) => {
                            let offset = label_to_offset.get(&label)?;
                            Function(*offset)
                        },
                        PreValue::Null => Null,
                        PreValue::Undefined => Undefined,
                    };
                    instructions.push(Val(val));
                }
                PreInstruction::FnCall => instructions.push(FnCall),
                PreInstruction::FnRet => instructions.push(FnRet),
                PreInstruction::PushToNext(n) => instructions.push(PushToNext(n)),
                PreInstruction::NewStack => instructions.push(NewStack),
                PreInstruction::ClearStack => instructions.push(ClearStack),
                PreInstruction::DelStack => instructions.push(DelStack),

                PreInstruction::Add => instructions.push(Add),
                PreInstruction::Div => instructions.push(Div),
                PreInstruction::Sub => instructions.push(Sub),
                PreInstruction::Mul => instructions.push(Mul),
                PreInstruction::Pow => instructions.push(Pow),
                PreInstruction::Mod => instructions.push(Mod),
                PreInstruction::Equal => instructions.push(Equal),
                PreInstruction::NotEqual => instructions.push(NotEqual),
                PreInstruction::GreaterThan => instructions.push(GreaterThan),
                PreInstruction::GreaterThanOrEqual => instructions.push(GreaterThanOrEqual),
                PreInstruction::LessThan => instructions.push(LessThan),
                PreInstruction::LessThanOrEqual => instructions.push(LessThanOrEqual),
                PreInstruction::And => instructions.push(And),
                PreInstruction::Or => instructions.push(Or),
                PreInstruction::Minus => instructions.push(Minus),
                PreInstruction::Plus => instructions.push(Plus),
                PreInstruction::Not => instructions.push(Not),
            }
        }
        Some(instructions)
    }
}
