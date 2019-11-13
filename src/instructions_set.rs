use crate::ast::Expr;
use crate::instructions::{Value,Instruction,Address};

/// Build the intructions needed to run a `return` expressions
/// If no expression follows the `return`, a Value::Null is returned
/// So `return <expr>;` is compiled as:
///     <expr>
///     PushToNext(1)
///     DelStack
///     FnRet
fn return_to_instructions(mut sub_instructions: Vec<Vec<Instruction>>) -> Option<Vec<Instruction>> {
    use Instruction::*;
    let mut instructions = Vec::new();
    match sub_instructions.pop() {
        Some(sub_instr) => {
            instructions.extend(sub_instr);
        },
        None => {
            // a function that returns nothing actually 
            // returns Undefined
            instructions.push(Val(Value::Undefined));
        }
    }
    instructions.push(PushToNext(1));
    instructions.push(DelStack);
    instructions.push(FnRet);
    Some(instructions)
}

/// Build the intructions needed to run a `block` expressions
/// each block creates a new value stack,
/// each expression in a block must operate on an empty stack
///  `{ <expr_a>; <expr_b>; }` is compiled as
///     NewStack
///     <expr_a>
///     ClearStack
///     <expr_b>
///     DelStack
fn block_to_instructions(mut sub_instructions: Vec<Vec<Instruction>>) -> Option<Vec<Instruction>> {
    use Instruction::*;
    let mut instructions: Vec<Instruction> = Vec::new();
    instructions.push(NewStack);
    for (i, sub_inst) in sub_instructions.into_iter().enumerate() {
        if i > 0 {
            // each expression must run in a clear stack
            instructions.push(ClearStack);
        }
        instructions.extend(sub_inst);
    }
    instructions.push(DelStack);
    Some(instructions)
}

/// Build the intructions needed to run an `If` expressions
/// So `if (<expr_cond>) { <expr_true> } else { <expr_false> }` is compiled as:
///     <expr_cond>
///     GotoIf 'label_true'
///     <expr_false>
///     Goto 'end'
///     Label 'label_true'
///     <expr_true>
///     Label 'end'
fn if_to_instructions(mut sub_instructions: Vec<Vec<Instruction>>, label_counter: &mut usize) -> Option<Vec<Instruction>> {
    use Instruction::*;
    let mut instructions = Vec::new();
    let true_block_addr = Address::Mark(*label_counter);
    *label_counter += 1;
    let end = Address::Mark(*label_counter);
    *label_counter += 1;

    sub_instructions.reverse();
    let cond_block = sub_instructions.pop()?;
    let true_block = sub_instructions.pop()?;
    let false_block = sub_instructions.pop();

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

/// Build the instructions needed to run a LetDecl expression.
/// * `let id = <expr>`  is compiled as:
///     <expr>
///     NewRef 'id'
///     Store 'id'
/// *`let id;`  is compiled as:
///     NewRef 'id'
fn let_to_instructions(id: &str, mut sub_instructions: Vec<Vec<Instruction>>) -> Option<Vec<Instruction>> {
    use Instruction::*;
    let mut instructions = Vec::new();
    if let Some(sub_inst) = sub_instructions.pop() {
        instructions.extend(sub_inst);
        instructions.push(NewRef(id.into()));
        instructions.push(Store(id.into()));
    } else {
        instructions.push(NewRef(id.into()));
    }
    Some(instructions)
}

/// Build the instructions needed to run a Local expression.
/// `a`  is compiled as:
///     Load 'a'
fn local_to_instructions(id: &str) -> Option<Vec<Instruction>> {
    use Instruction::*;
    Some(vec![Load(id.into())])
}

/// Build the intructions needed to run a Call expression
/// So `<expr_called>(<expr_a>, <expr_b>);` is compiled as:
///     <expr_b>
///     <expr_a>        // order so the first poped is the first arg
///     PushToNext(2)   // push the 2 args
///     <expr_called>
///     FnCall
fn call_to_instructions(mut sub_instructions: Vec<Vec<Instruction>>) -> Option<Vec<Instruction>> {
    use Instruction::*;
    let mut instructions = Vec::new();
    // set in order <arg 2>, <arg 1>, <arg 0>, <expr called> 
    sub_instructions.reverse();
    let expr_called = sub_instructions.pop()?;
    let arg_nb = sub_instructions.len();
    for sub_inst in sub_instructions {
        // evaluate args
        instructions.extend(sub_inst);
    }
    // push args to the next stack
    if arg_nb > 0 {
        instructions.push(PushToNext(arg_nb));
    }
    // evaluate the called expression
    instructions.extend(expr_called);
    // perform the Call itself
    instructions.push(FnCall);

    Some(instructions)
}

/// Build the intructions needed to run a FunctionDecl expression
/// So `function id(a, b) { <expr> }` is compiled as:
///     Goto(end)       // skip the function block if were not calling it
///     Label(start)    // address of the function
///     NewStack
///     NewRef 'a'      // args
///     Store 'a'
///     NewRef 'b'
///     Store 'b'
///     <expr>              // the function block instructions
///     Value(Undefined)    // Return a value no matter what
///     PushToNext(1)
///     DelStack
///     FnRet               // quit the function evaluation 
///     Label(end)          // the piece of code that is executed when the function is 
///                         // declared
///     Value(Address(start)) // Put the address of the function into a var `id`
///     NewRef 'id'
///     Store 'id'
///     
/// Note: this function will remove the `NewStack` and `DelStack` of the function block
/// TODO: replace `name` by an expression
fn function_decl_to_instructions(
    mut sub_instructions: Vec<Vec<Instruction>>,
    name: &str,
    args: &Vec<String>,
    label_counter: &mut usize) -> Option<Vec<Instruction>> {
    use Instruction::*;
    let mut instructions = Vec::new();
    // set the address of the function start and end
    let start = Address::Mark(*label_counter);
    *label_counter += 1;
    let end = Address::Mark(*label_counter);
    *label_counter += 1;
    // avoid evaluating the function if were not calling it
    instructions.push(Goto(end));
    // set the `start` label here
    instructions.push(Label(start)); 
    // create a new stack
    instructions.push(NewStack); 
    
    // load args
    for arg in args {
        instructions.push(NewRef(arg.clone()));
        instructions.push(Store(arg.clone()));
    }
    
    // process the function block
    let mut block = sub_instructions.pop()?;
    //delete the NewStack, its redondant
    block.remove(0);
    //delete the DelStack, its redondant
    let _ = block.pop()?;
    // insert the instruction block
    instructions.extend(block);

    // insert a return statement in case none are defined in the block
    instructions.push(Val(Value::Undefined));
    instructions.push(PushToNext(1));
    instructions.push(DelStack);
    instructions.push(FnRet);
    
    // set the end address
    instructions.push(Label(end)); 

    // set the variable that will hold the function address
    instructions.push(Val(Value::Function(start)));
    instructions.push(NewRef(name.into()));
    instructions.push(Store(name.into()));

    Some(instructions)
}


#[derive(Debug)]
/// A struct that represents an instruction set, ie: a compiled Abstract Syntax Tree
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
            for i in 0..sub_expr_count {
                sub_instructions.push(instructions.pop()?);
            }
            use Expr::*;
            use Instruction::*;
            let insts: Vec<Instruction> = match *node {
                Const(c) => vec![Val(Value::from_const(c))],
                Return(_) => return_to_instructions(sub_instructions)?,
                If(..) => if_to_instructions(sub_instructions, &mut label_counter)?,
                Block(..) => block_to_instructions(sub_instructions)?,
                LetDecl(name, _) => let_to_instructions(name, sub_instructions)?,
                Local(id) => local_to_instructions(id)?,
                Call(..) => call_to_instructions(sub_instructions)?,
                FunctionDecl(name, args, _block) => function_decl_to_instructions(
                    sub_instructions, name, args, &mut label_counter)?,
                //BinaryOp(op, ..) => { },
                //UnaryOp(op, a) => { },
                //WhileLoop(cond, block) => { },
                _ => { Vec::new()}
            };
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
    use crate::instructions::{Instruction,Value,Address};

    #[test]
    fn test_if_instruction_set() {
        use Address::*;
        use Instruction::*;
        use Value::*;
        let ast = Ast::from_str(r#"
        if (true) {
          "true_block";
        } else {
          "false_block";
        }"#).unwrap();
        assert_eq!(
            InstructionSet::new(&ast.root).unwrap().instructions,
			vec![
                Val(Bool(true)),
                GotoIf(Mark(0)),
                NewStack,
                Val(Str("false_block".into())),
                DelStack,
                Goto(Mark(1)),
                Label(Mark(0)),
                NewStack,
                Val(Str("true_block".into())),
                DelStack,
                Label(Mark(1)),
            ],
		);
    }

    #[test]
    fn test_let_instruction_set() {
        use Instruction::*;
        use Value::*;
        let ast = Ast::from_str("let some_ref;").unwrap();
        assert_eq!(
            InstructionSet::new(&ast.root).unwrap().instructions,
            vec![
                NewRef("some_ref".into()),
            ],
        );
        let ast = Ast::from_str("let some_ref = true;").unwrap();
        assert_eq!(
            InstructionSet::new(&ast.root).unwrap().instructions,
            vec![
                Val(Bool(true)),
                NewRef("some_ref".into()),
                Store("some_ref".into()),
            ],
        );
    }

    #[test]
    fn test_local_instruction_set() {
        use Instruction::*;
        use Value::*;
        let ast = Ast::from_str("a;").unwrap();
        assert_eq!(
            InstructionSet::new(&ast.root).unwrap().instructions,
            vec![
                Load("a".into()),
            ],
        );
    }
    #[test]
    fn test_block_instruction_set() {
        use Instruction::*;
        use Value::*;
        let ast = Ast::from_str("{ 1; 2; 3; }").unwrap();
        assert_eq!(
            InstructionSet::new(&ast.root).unwrap().instructions,
            vec![
                NewStack,
                Val(Num(1.)),
                ClearStack,
                Val(Num(2.)),
                ClearStack,
                Val(Num(3.)),
                DelStack,
            ],
        );
    }
    #[test]
    fn test_call_instruction_set() {
        use Instruction::*;
        use Value::*;
        // test with args
        let ast = Ast::from_str("foo(a, b)").unwrap();
        assert_eq!(
            InstructionSet::new(&ast.root).unwrap().instructions,
            vec![
                Load("b".into()),
                Load("a".into()),
                PushToNext(2),
                Load("foo".into()),
                FnCall,
            ],
        );
        // test without args
        let ast = Ast::from_str("foo()").unwrap();
        assert_eq!(
            InstructionSet::new(&ast.root).unwrap().instructions,
            vec![
                Load("foo".into()),
                FnCall,
            ],
        );
    }
    #[test]
    fn test_function_decl_instruction_set() {
        use Instruction::*;
        use Address::*;
        use Value::*;
        let ast = Ast::from_str("function foo(a, b) { return true; }").unwrap();
        assert_eq!(
            InstructionSet::new(&ast.root).unwrap().instructions,
            vec![
                Goto(Mark(1)),  // skip function block if we're not calling it
                Label(Mark(0)), // begin address
                NewStack,
                NewRef("a".into()), // first arg 
                Store("a".into()),
                NewRef("b".into()), // 2nd arg
                Store("b".into()),
                Val(Bool(true)), // function block
                PushToNext(1),   
                DelStack,
                FnRet,
                Val(Undefined),  // backup return call
                PushToNext(1),
                DelStack,
                FnRet,
                Label(Mark(1)),  // end address
                Val(Function(Mark(0))), // put function address into a `foo` var
                NewRef("foo".into()),
                Store("foo".into()),
            ],
        );
    }
}

