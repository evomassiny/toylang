use crate::builtins::{
    LexicalLabel,
};
use crate::compiler::proto_instructions::{
    ProtoValue,
    ProtoInstruction,
    ProtoInstruction::*,
    LabelGenerator,
};
use crate::ast::{
    Const,
    BinaryOp,
    BinaryOp::*,
    NumericalOp,
    ComparisonOp,
    LogicalOp,
    UnaryOp,
};


/// Build the intructions needed to run a `Const` expression
pub fn compile_const(c: &Const) -> Option<Vec<ProtoInstruction>> {
    Some(vec![Val(ProtoValue::from_const(c))])
}

/// Build the intructions needed to run a `Break` expression
pub fn compile_break() -> Option<Vec<ProtoInstruction>> {
    Some(vec![Break])
}

/// Build the intructions needed to run a `Continue` expression
pub fn compile_continue() -> Option<Vec<ProtoInstruction>> {
    Some(vec![PopToLoopCtx, Continue])
}

/// Build the intructions needed to run a `return` expression
/// If no expression follows the `return`, a ProtoValue::Null is returned
/// So `return <expr>;` is compiled as:
///     <expr>
///     PushToNext(1)
///     FnRet
pub fn compile_return(mut sub_instructions: Vec<Vec<ProtoInstruction>>) -> Option<Vec<ProtoInstruction>> {
    let mut instructions = Vec::new();
    match sub_instructions.pop() {
        Some(sub_instr) => {
            instructions.extend(sub_instr);
        },
        None => {
            // a function that returns nothing actually 
            // returns Undefined
            instructions.push(Val(ProtoValue::Undefined));
        }
    }
    instructions.push(PushToNext(1));
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
pub fn compile_block(sub_instructions: Vec<Vec<ProtoInstruction>>) -> Option<Vec<ProtoInstruction>> {
    let mut instructions: Vec<ProtoInstruction> = Vec::new();
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
///     AddrLabel 'label_true'
///     <expr_true>
///     AddrLabel 'end'
pub fn compile_if(mut sub_instructions: Vec<Vec<ProtoInstruction>>, labels: &mut LabelGenerator) -> Option<Vec<ProtoInstruction>> {
    let mut instructions = Vec::new();
    let true_block_label = labels.jump_to();
    let end = labels.jump_to();

    sub_instructions.reverse();
    let cond_block = sub_instructions.pop()?;
    let true_block = sub_instructions.pop()?;
    let false_block = sub_instructions.pop();

    // conditions
    instructions.extend(cond_block);
    instructions.push(GotoIf(true_block_label.addr));
    // false block
    if let Some(false_block) = false_block {
        instructions.extend(false_block);
    }
    instructions.push(Goto(end.addr));
    // true block
    instructions.push(AddrLabel(true_block_label));
    instructions.extend(true_block);
    // end (needed after the execution of the false block)
    instructions.push(AddrLabel(end));

    Some(instructions)
}

/// Build the instructions needed to run a LetDecl expression.
/// * `let id = <expr>`  is compiled as:
///     <expr>
///     NewRef 'id' 'ctx'
///     Store 'id' 'ctx'
/// *`let id;`  is compiled as:
///     NewRef 'id' 'ctx'
pub fn compile_let(id: &str, mut sub_instructions: Vec<Vec<ProtoInstruction>>) -> Option<Vec<ProtoInstruction>> {
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
pub fn compile_local(id: &str) -> Option<Vec<ProtoInstruction>> {
    Some(vec![Load(id.into())])
}

/// Build the intructions needed to run a Call expression
/// So `<expr_called>(<expr_a>, <expr_b>);` is compiled as:
///     <expr_b>
///     <expr_a>        // order so the first poped is the first arg
///     PushToNext(2)   // push the 2 args
///     <expr_called>
///     FnCall
pub fn compile_call(mut sub_instructions: Vec<Vec<ProtoInstruction>>) -> Option<Vec<ProtoInstruction>> {
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
///     Goto(end)               // skip the function block if were not calling it
///     AddrLabel(start)        // address of the function
///     NewContext(ctx)         // creates a new context 
///     NewRef 'a'              // args
///     Store 'a'
///     NewRef 'b'
///     Store 'b'
///     <expr>                  // the function block instructions
///     ProtoValue(Undefined)   // Return a value no matter what
///     PushToNext(1)
///     FnRet                   // quit the function evaluation 
///     AddrLabel(end)          // the piece of code that is executed when the function is 
///                             // declared
///     NewFunction(start)      // creates a new Function object
///     NewRef 'id'             // creates a new Reference `id`
///     Store 'id'              // stores the function object into it
///     
pub fn compile_function_decl(
    mut sub_instructions: Vec<Vec<ProtoInstruction>>,
    name: &str,
    args: &Vec<String>,
    labels: &mut LabelGenerator,
    ctx: LexicalLabel,
    ) -> Option<Vec<ProtoInstruction>> {
    let mut instructions = Vec::new();
    // set the address of the function start and end
    let start = labels.begin_function();
    let end = labels.end_function();
    // avoid evaluating the function if were not calling it
    instructions.push(Goto(end.addr));
    // set the `start` label here
    instructions.push(AddrLabel(start)); 
    // Build a new Context
    instructions.push(NewContext(ctx.clone())); 
    
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
    match instructions.last() {
        Some(FnRet) => {},
        _ => {
            instructions.push(Val(ProtoValue::Undefined));
            instructions.push(PushToNext(1));
            instructions.push(FnRet);
        },
    }
    
    // set the end address
    instructions.push(AddrLabel(end)); 

    // create a function object
    instructions.push(NewFunction(start.addr));
    // set the variable that will hold the function object
    instructions.push(NewRef(name.into()));
    instructions.push(Store(name.into()));

    Some(instructions)
}

/// Build the intructions needed to run a numerical BinaryOp
/// So `<expr_a> + <expr_b>` is compiled as:
///     <expr_b>
///     <expr_a>
///     Add
pub fn compile_numerical_op(op: &NumericalOp, mut sub_instructions: Vec<Vec<ProtoInstruction>>) -> Option<Vec<ProtoInstruction>> {
    let mut instructions = Vec::new();
    // last instruction must be at the bottom of the stack
    instructions.extend(sub_instructions.pop()?);
    instructions.extend(sub_instructions.pop()?);
    // append the actual Operation instruction
    let operation_instruction = match *op {
        NumericalOp::Add => ProtoInstruction::Add,
        NumericalOp::Sub => ProtoInstruction::Sub,
        NumericalOp::Div => ProtoInstruction::Div,
        NumericalOp::Mul => ProtoInstruction::Mul,
        NumericalOp::Pow => ProtoInstruction::Pow,
        NumericalOp::Mod => ProtoInstruction::Mod,
    };
    instructions.push(operation_instruction);
    Some(instructions)
}

/// Build the intructions needed to run a comparison BinaryOp
/// So `<expr_a> > <expr_b>` is compiled as:
///     <expr_b>
///     <expr_a>
///     GreaterThan
pub fn compile_comparison_op(op: &ComparisonOp, mut sub_instructions: Vec<Vec<ProtoInstruction>>) -> Option<Vec<ProtoInstruction>> {
    let mut instructions = Vec::new();
    // last instruction must be at the bottom of the stack
    instructions.extend(sub_instructions.pop()?);
    instructions.extend(sub_instructions.pop()?);
    // append the actual Operation instruction
    let operation_instruction = match *op {
        ComparisonOp::Equal => ProtoInstruction::Equal,
        ComparisonOp::NotEqual => ProtoInstruction::NotEqual,
        ComparisonOp::GreaterThan => ProtoInstruction::GreaterThan,
        ComparisonOp::GreaterThanOrEqual => ProtoInstruction::GreaterThanOrEqual,
        ComparisonOp::LessThan => ProtoInstruction::LessThan,
        ComparisonOp::LessThanOrEqual => ProtoInstruction::LessThanOrEqual,
    };
    instructions.push(operation_instruction);
    Some(instructions)
}

/// Build the intructions needed to run a Logical binary operation, eg `&&` and `||`.
/// Handle shortcut evaluation
/// * So `<expr_a> && <expr_b>` is compiled as:
///     <expr_a>
///     GotoIf 'right_hand'
///     Goto 'end'
///     AddrLabel 'right_hand'
///     <expr_b>
///     And
///     AddrLabel 'end'
/// * So `<expr_a> || <expr_b>` is compiled as:
///     <expr_a>
///     GotoIf 'end'
///     <expr_b>
///     Or
///     AddrLabel 'end'
pub fn compile_logical_op(op: &LogicalOp, mut sub_instructions: Vec<Vec<ProtoInstruction>>, labels: &mut LabelGenerator) -> Option<Vec<ProtoInstruction>> {
    // avoid evaluating the function if were not calling it
    let right_hand = sub_instructions.pop()?;
    // process the left hand of the `and`
    let mut instructions = sub_instructions.pop()?;
    // prepare the `end` label
    let end = labels.jump_to();
    // append the actual Operation instructions
    match *op {
        LogicalOp::And => {
            let right_label = labels.jump_to();
            instructions.push(GotoIf(right_label.addr)); 
            instructions.push(Goto(end.addr)); 
            instructions.push(AddrLabel(right_label)); 
            instructions.extend(right_hand);
            instructions.push(And); 

        },
        LogicalOp::Or => {
            instructions.push(GotoIf(end.addr)); 
            instructions.extend(right_hand);
            instructions.push(Or); 
        },
    };
    instructions.push(AddrLabel(end)); 

    Some(instructions)
}


/// Build the intructions needed to run a Assign BinaryOp
/// So `id = <expr>` is compiled as:
///     <expr>
///     Assign(id)
pub fn compile_assign(name: &str, mut sub_instructions: Vec<Vec<ProtoInstruction>>) -> Option<Vec<ProtoInstruction>> {
    let mut instructions = sub_instructions.pop()?;
    instructions.push(Store(name.into()));
    Some(instructions)
}

/// Build the intructions needed to run an UnaryOp
/// * So `! <expr>` is compiled as:
///     <expr>
///     Not
/// * So `+ <expr>` is compiled as:
///     <expr>
///     Plus
/// * So `- <expr>` is compiled as:
///     <expr>
///     Not
/// * So `id ++` is compiled as:
///     Load(id)
///     Load(id)
///     Val(Num(1.))
///     Add
///     Store(id)
/// * So `++ id` is compiled as:
///     Load(id)
///     Val(Num(1.))
///     Add
///     Store(id)
///     Load(id)
pub fn compile_unary_op(op: &UnaryOp, mut sub_instructions: Vec<Vec<ProtoInstruction>>) -> Option<Vec<ProtoInstruction>> {
    let mut operand_insts = sub_instructions.pop()?;
    let insts: Vec<ProtoInstruction> = match *op {
        UnaryOp::Minus => vec![ProtoInstruction::Minus],
        UnaryOp::Plus => vec![ProtoInstruction::Plus],
        UnaryOp::Not => vec![ProtoInstruction::Not],
        UnaryOp::PreInc => {
            let id = match &operand_insts[0] {
                ProtoInstruction::Load(id) => id.clone(),
                _ => return None,
            };
            vec![
                ProtoInstruction::Val(ProtoValue::Num(1.0)),
                ProtoInstruction::Add,
                ProtoInstruction::Store(id.clone()),
                ProtoInstruction::Load(id.clone()),
            ]
        },
        UnaryOp::PreDec => {
            let id = match &operand_insts[0] {
                ProtoInstruction::Load(id) => id.clone(),
                _ => return None,
            };
            vec![
                ProtoInstruction::Val(ProtoValue::Num(1.0)),
                ProtoInstruction::Sub,
                ProtoInstruction::Store(id.clone()),
                ProtoInstruction::Load(id.clone()),
            ]
        },
        UnaryOp::PostInc => {
            let id = match &operand_insts[0] {
                ProtoInstruction::Load(id) => id.clone(),
                _ => return None,
            };
            vec![
                ProtoInstruction::Load(id.clone()),
                ProtoInstruction::Val(ProtoValue::Num(1.0)),
                ProtoInstruction::Add,
                ProtoInstruction::Store(id.clone()),
            ]
        },
        UnaryOp::PostDec => {
            let id = match &operand_insts[0] {
                ProtoInstruction::Load(id) => id.clone(),
                _ => return None,
            };
            vec![
                ProtoInstruction::Load(id.clone()),
                ProtoInstruction::Val(ProtoValue::Num(1.0)),
                ProtoInstruction::Sub,
                ProtoInstruction::Store(id.clone()),
            ]
        },
    };
    operand_insts.extend(insts);
    Some(operand_insts)
}

/// Build the intructions needed to run WhileLoop
/// * So `while (<cond_expr>) { <block_expr>}` is compiled as:
///     NewLoopCtx
///     Label 'start_loop'
///     <cond_expr>
///     GotoIf 'block'
///     Goto 'end'
///     Label 'block'
///     <block_expr>
///     Goto 'start_loop'
///     Label 'end_loop'
///     DelLoopCtx
pub fn compile_while(mut sub_instructions: Vec<Vec<ProtoInstruction>>, labels: &mut LabelGenerator) -> Option<Vec<ProtoInstruction>> {
    // build label
    let begin = labels.begin_loop();
    let block_label = labels.jump_to();
    let end = labels.end_loop();
    // gather expressions
    let block = sub_instructions.pop()?;
    let condition = sub_instructions.pop()?;
    // build the instruction set
    let mut instructions = Vec::new();
    instructions.push(NewLoopCtx);
    instructions.push(AddrLabel(begin));
    instructions.extend(condition);
    instructions.push(GotoIf(block_label.addr));
    instructions.push(Goto(end.addr));
    instructions.push(AddrLabel(block_label));
    instructions.extend(block);
    instructions.push(Goto(begin.addr));
    instructions.push(AddrLabel(end));
    instructions.push(DelLoopCtx);
    Some(instructions)
}

/// Build the intructions needed to run a BinaryOp expression
/// see:
/// * `compile_numerical_op`
/// * `compile_comparison_op`
/// * `compile_logical_op`
/// for details
pub fn compile_binary_op(op: &BinaryOp, sub_instructions: Vec<Vec<ProtoInstruction>>, labels: &mut LabelGenerator) -> Option<Vec<ProtoInstruction>> {
    match op {
        Numerical(num_op) => compile_numerical_op(num_op, sub_instructions),
        Comparison(comp_op) => compile_comparison_op(comp_op, sub_instructions),
        Logical(logical_op) => compile_logical_op(logical_op, sub_instructions, labels),
    }
}
    
#[cfg(test)]
mod test {
    use std::str::FromStr;
    use crate::ast::Ast;
    use crate::compiler::compiler::Compiler;
    use crate::compiler::proto_instructions::{ProtoValue,ProtoInstruction,Addr,AddrKind};
    use ProtoInstruction::*;
    use ProtoValue::*;

    #[test]
    fn test_if_instruction_set() {
        let ast = Ast::from_str(r#"
        if (true) {
          "true_block";
        } 
        "#).unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Bool(true)),
                GotoIf(0),
                Goto(1),
                AddrLabel(Addr { addr: 0, kind: AddrKind::Jump }),
                NewStack,
                Val(Str("true_block".into())),
                DelStack,
                AddrLabel(Addr { addr: 1, kind: AddrKind::Jump }),
            ],
		);
        let ast = Ast::from_str(r#"
        if (true) {
          "true_block";
        } else {
          "false_block";
        }"#).unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Bool(true)),
                GotoIf(0),
                NewStack,
                Val(Str("false_block".into())),
                DelStack,
                Goto(1),
                AddrLabel(Addr { addr: 0, kind: AddrKind::Jump }),
                NewStack,
                Val(Str("true_block".into())),
                DelStack,
                AddrLabel(Addr { addr: 1, kind: AddrKind::Jump }),
            ],
		);
    }

    #[test]
    fn test_let_instruction_set() {
        let ast = Ast::from_str("let some_ref;").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                NewRef("some_ref".into()),
            ],
        );
        let ast = Ast::from_str("let some_ref = true;").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Bool(true)),
                NewRef("some_ref".into()),
                Store("some_ref".into()),
            ],
        );
    }

    #[test]
    fn test_local_instruction_set() {
        let ast = Ast::from_str("a;").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Load("a".into()),
            ],
        );
    }
    #[test]
    fn test_block_instruction_set() {
        let ast = Ast::from_str("{ 1; 2; 3; }").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
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
        // test with args
        let ast = Ast::from_str("foo(a, b)").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Load("b".into()),
                Load("a".into()),
                PushToNext(2),
                Load("foo".into()),
                FnCall,
            ],
        );
        // test with one arg
        let ast = Ast::from_str("foo(a)").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Load("a".into()),
                PushToNext(1),
                Load("foo".into()),
                FnCall,
            ],
        );
        // test without args
        let ast = Ast::from_str("foo()").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Load("foo".into()),
                FnCall,
            ],
        );
    }
    #[test]
    fn test_function_decl_instruction_set() {
        let ast = Ast::from_str("function foo(a, b) { return true; }").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Goto(1),  // skip function block if we're not calling it
                AddrLabel(Addr { addr: 0, kind: AddrKind::BeginFunction } ), // begin address
                NewContext("global__foo_0".into()),
                NewRef("a".into()), // first arg 
                Store("a".into()),
                NewRef("b".into()), // 2nd arg
                Store("b".into()),
                Val(Bool(true)), // function block
                PushToNext(1),   
                FnRet,
                AddrLabel(Addr { addr: 1, kind: AddrKind::EndFunction } ), // begin address
                NewFunction(0), // put function address into a `foo` var
                NewRef("foo".into()),
                Store("foo".into()),
            ],
        );
        // test automatic "return undefined"
        let ast = Ast::from_str("function foo(a, b) { }").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Goto(1),  // skip function block if we're not calling it
                AddrLabel(Addr { addr: 0, kind: AddrKind::BeginFunction } ), // begin address
                NewContext("global__foo_0".into()),
                NewRef("a".into()), // first arg 
                Store("a".into()),
                NewRef("b".into()), // 2nd arg
                Store("b".into()),
                Val(Undefined),  // backup return call
                PushToNext(1),
                FnRet,
                AddrLabel(Addr { addr: 1, kind: AddrKind::EndFunction } ), // begin address
                NewFunction(0), // put function address into a `foo` var
                NewRef("foo".into()),
                Store("foo".into()),
            ],
        );
    }
    #[test]
    fn test_assign_instruction_set() {
        let ast = Ast::from_str("a = 2").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Num(2.)),
                Store("a".into()),
            ],
        );
    }
    #[test]
    fn test_numerical_binary_op_instruction_set() {
        let ast = Ast::from_str("1 + 2").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Num(2.)),
                Val(Num(1.)),
                Add,
            ],
        );
    }
    #[test]
    fn test_comparison_binary_op_instruction_set() {
        let ast = Ast::from_str("1 <= 2").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Num(2.)),
                Val(Num(1.)),
                LessThanOrEqual
            ],
        );
    }
    #[test]
    fn test_logical_binary_op_instruction_set() {
        // test AND
        let ast = Ast::from_str("true && false").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Bool(true)),
                GotoIf(1), // success path
                Goto(0), // failure path
                AddrLabel(Addr { addr: 1, kind: AddrKind::Jump }),
                Val(Bool(false)),
                And,
                AddrLabel(Addr { addr: 0, kind: AddrKind::Jump }),
            ],
        );
        // test OR
        let ast = Ast::from_str("true || false").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Bool(true)),
                GotoIf(0), // success path
                Val(Bool(false)),
                Or,
                AddrLabel(Addr { addr: 0, kind: AddrKind::Jump }),
            ],
        );
    }
    #[test]
    fn test_unary_op_instruction_set() {
        let ast = Ast::from_str("!true").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Bool(true)),
                Not,
            ],
        );
        let ast = Ast::from_str("+1").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Num(1.)),
                Plus,
            ],
        );
        let ast = Ast::from_str("-1").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Val(Num(1.)),
                Minus,
            ],
        );
        // test pre increment operator
        let ast = Ast::from_str("++a").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Load("a".into()),
                Val(Num(1.)),
                Add,
                Store("a".into()),
                Load("a".into()),
            ],
        );
        // test pre decrement operator
        let ast = Ast::from_str("--a").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Load("a".into()),
                Val(Num(1.)),
                Sub,
                Store("a".into()),
                Load("a".into()),
            ],
        );
        // test post increment operator
        let ast = Ast::from_str("a++").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Load("a".into()),
                Load("a".into()),
                Val(Num(1.)),
                Add,
                Store("a".into()),
            ],
        );
        // test post decrement operator
        let ast = Ast::from_str("a--").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                Load("a".into()),
                Load("a".into()),
                Val(Num(1.)),
                Sub,
                Store("a".into()),
            ],
        );
    }
    #[test]
    fn test_while_instruction_set() {
        let ast = Ast::from_str("while(a < 2) { a = a + 1; }").unwrap();
        assert_eq!(
            Compiler::preprocess(&ast.root).unwrap(),
            vec![
                NewLoopCtx,
                AddrLabel(Addr { addr: 0, kind: AddrKind::BeginLoop }), // begin
                Val(Num(2.)),
                Load("a".into()),
                LessThan,
                GotoIf(1),
                Goto(2),
                AddrLabel(Addr { addr: 1, kind: AddrKind::Jump }), // block 
                NewStack,
                Val(Num(1.)),
                Load("a".into()),
                Add,
                Store("a".into()),
                DelStack,
                Goto(0),
                AddrLabel(Addr { addr: 2, kind: AddrKind::EndLoop }), // block 
                DelLoopCtx,
            ],
        );
    }
}

