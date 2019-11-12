use crate::ast::Expr;
use crate::instructions::{Value,Instruction,Address};

/// Build the intructions needed to run a `return` expressions
/// If no expression follows the `return`, a Value::Null is returned
/// So `return <expr>;` is compiled as:
/// <expr>
/// PushStack
/// FnRet
fn return_to_instructions(mut sub_instructions: Vec<Vec<Instruction>>, _label_counter: &mut usize) -> Option<Vec<Instruction>> {
    use Instruction::*;
    let mut instructions = Vec::new();
    match sub_instructions.pop() {
        Some(sub_instr) => {
            instructions.extend(sub_instr);
            instructions.push(PushStack); 
        },
        None => {
            // a function that returns nothing actually 
            // returns null
            instructions.push(Val(Value::Null));
            instructions.push(PushStack); 
        }
    }
    instructions.push(FnRet);
    Some(instructions)
}

/// Build the intructions needed to run an `If` expressions
/// So `if (<expr_cond>) { <expr_true> } else { <expr_false> }` is compiled as:
/// <expr_cond>
/// GotoIf 'label_true'
/// <expr_false>
/// Goto 'end'
/// Label 'label_true'
/// <expr_true>
/// Label 'end'
fn if_to_instructions(mut sub_instructions: Vec<Vec<Instruction>>, label_counter: &mut usize) -> Option<Vec<Instruction>> {
    use Instruction::*;
    let mut instructions = Vec::new();
    let true_block_addr = Address::Mark(*label_counter);
    *label_counter += 1;
    let end = Address::Mark(*label_counter);
    *label_counter += 1;

    let cond_block = sub_instructions.pop()?;
    let true_block = sub_instructions.pop()?;
    let false_block = sub_instructions;

    // conditions
    instructions.extend(cond_block);
    instructions.push(GotoIf(true_block_addr));
    // false block
    if let Some(false_block) = false_block {
        instructions.extend(false_block);
        instructions.push(Goto(end));
    }
    // true block
    instructions.push(Label(true_block_addr));
    instructions.extend(true_block);
    // end (needed after the execution of the false block)
    instructions.push(Label(end));

    Some(instructions)
}

#[derive(Debug)]
pub struct InstructionSet {
    instructions: Vec<Instruction>
}

impl InstructionSet {
    pub fn new(expression: &Expr) -> Option<Self> {
        let mut instructions: Vec<Vec<Instruction>> = Vec::new();
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
            for _ in 0..sub_expr_count {
                sub_instructions.push(instructions.pop()?);
            }
            use Expr::*;
            use Instruction::*;
            let insts: Vec<Instruction> = match *node {
                Const(c) => vec![Val(Value::from_const(c))],
                Return(_) => return_to_instructions(sub_instructions, &mut label_counter)?,
                If(..) => if_to_instructions(sub_instructions, &mut label_counter)?,
                //BinaryOp(op, ..) => { },
                //UnaryOp(op, a) => { },
                //Block(exprs) => {},
                //Call(id, args) => { },
                //WhileLoop(cond, block) => { },
                //Local(id) => {},
                //FunctionDecl(_name, _args, block) => { },
                //LetDecl(_name, e) => { },
                _ => { Vec::new()}
            };
            dbg!(&insts);
            if insts.len() > 0 {
                instructions.push(insts);
            }

            // pop the current node, it's been solved.
            let _ = nodes.pop();
            let _ = dep_count.pop();
            
        }
        Some(Self { instructions: instructions.pop().unwrap() })
    }
}

mod test {
    use crate::ast::Ast;
    use crate::instructions_set::InstructionSet;
    use crate::instructions::Instruction;

    #[test]
    fn test_instruction_set() {
        let ast = Ast::from_str(r"a = 1 + 1;").unwrap();
        dbg!(InstructionSet::new(&ast.root));
    }
}

