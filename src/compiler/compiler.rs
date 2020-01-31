use std::collections::HashMap;
use crate::ast::Expr;
use crate::compiler::proto_instructions::{ProtoValue,ProtoInstruction,LabelGenerator,Addr,AddrKind};
use crate::compiler::expression_compilers as ec;
use crate::compiler::instructions::Instruction;
use crate::builtins::{Value,FnKind};


/// A Node in the AST
pub struct AstNode <'n> {
    /// An Ast Expression
    pub expression: &'n Expr,
    /// how many sub expressions `expressions` holds.
    pub dependancies: usize,
}

/// An Iterator over an Abstract Syntaxe Tree
/// It yields every nodes of a tree from right to left,
/// starting from the leaves.
/// TODO: it also keep track of the `scope` of each node
pub struct AstTraverser <'iter>{
    // the node stack we are traversing
    // each element is one level deeper (in the AST)
    // than the previous one.
    nodes: Vec<&'iter Expr>,
    // the index of the node we are crawling
    // for each level 
    crawling_idx: Vec<usize>,
}
impl <'iter> AstTraverser <'iter> {

    /// Build an iterator over the AST starting from `Expr`
    pub fn new(expression: &'iter Expr) -> Self {
        Self {
            nodes: vec![expression],
            crawling_idx: vec![0],
        }
    }
}
impl <'iter> Iterator for AstTraverser <'iter> {
    type Item = AstNode<'iter>;

    /// Traverses a tree from right to left,
    /// assert that every AST node is yielded right after 
    /// its sub-AST node
    fn next(&mut self) -> Option<Self::Item> {

        while self.nodes.len() > 0 {
            let idx = self.nodes.len() -1;

            // Crawl sub nodes first
            let sub_count = self.nodes[idx].count_sub_expressions();
            if self.crawling_idx[idx] < sub_count {
                self.crawling_idx.push(0);
                // assert that we tarverse the tree from right to left
                let next_idx = sub_count - self.crawling_idx[idx] - 1;
                if let Some(sub_node) = self.nodes[idx].get_sub_expression(next_idx) {
                    self.nodes.push(&sub_node);
                }
                self.crawling_idx[idx] += 1;
                continue;
            }

            // yield the current node, 
            // at this point each of its sub-nodes has been yielded
            let crawled_node = self.nodes.pop()?;
            let _ = self.crawling_idx.pop();

            return Some(AstNode {
                expression: crawled_node,
                dependancies: sub_count,
            });
            
        }
        None
    }
}

#[derive(Debug)]
pub struct Compiler {}

impl Compiler {

    /// compile an AST into a vector of ProtoInstruction
    /// an intermediate representation of the actual Instruction set
    /// with unsolved address (labels)
    pub fn preprocess(expression: &Expr) -> Option<Vec<ProtoInstruction>> {
        let mut instructions: Vec<Vec<ProtoInstruction>> = Vec::new();
        // a struct used to build uniq labels
        let mut labels = LabelGenerator::new();

        // traverse the AST for right to left, leaves first
        // and build an instructions list
        // executable by a simple stack machine
        for AstNode { expression, dependancies } in AstTraverser::new(expression) {

            // Collect the sub-expression instructions, if any
            let mut sub_instructions = Vec::new();
            for _ in 0..dependancies {
                sub_instructions.push(instructions.pop()?);
            }

            // Build the intruction set needed to evaluate the current expression
            use Expr::*;
            let insts: Vec<ProtoInstruction> = match expression {
                Const(c) => ec::preprocess_const(c)?,
                Return(_) => ec::preprocess_return(sub_instructions)?,
                If(..) => ec::preprocess_if(sub_instructions, &mut labels)?,
                Block(..) => ec::preprocess_block(sub_instructions)?,
                LetDecl(name, _) => ec::preprocess_let(name, sub_instructions)?,
                Local(id) => ec::preprocess_local(id)?,
                Assign(id, ..) => ec::assign_to_instructions(id, sub_instructions)?,
                Call(..) => ec::preprocess_call(sub_instructions)?,
                FunctionDecl(name, args, _block) => 
                    ec::preprocess_function_decl(sub_instructions, name, args, &mut labels)?,
                BinaryOp(op, ..) => 
                    ec::preprocess_binary_op(op, sub_instructions, &mut labels)?,
                UnaryOp(op, ..) => ec::preprocess_unary_op(op, sub_instructions)?,
                WhileLoop(..) => ec::preprocess_while(sub_instructions, &mut labels)?,
                Break => ec::preprocess_break()?,
                Continue => ec::preprocess_continue()?,
            };
            // Push those expression to the 
            if insts.len() > 0 {
                instructions.push(insts);
            }

        }
        Some(instructions.pop()?)
    }

    /// Solves labels in a proto instruction set into Instruction Addresses
    fn solve_labels(mut proto_instructions: Vec<ProtoInstruction>) -> Option<Vec<Instruction>> {
        use Instruction::*;
        // solve 'continue' as Goto labels
        // relies on the fact that a Beginloop label must be located *BEFORE* 
        // the *continue* expression
        let mut last_loop_start: usize = 0;
        for proto_intruction in proto_instructions.iter_mut() {
            match *proto_intruction {
                ProtoInstruction::AddrLabel( Addr { addr, kind: AddrKind::BeginLoop }) => {
                    // store the instruction index 
                    last_loop_start = addr;
                    continue;
                },
                ProtoInstruction::Continue => {},
                _ => continue,
            }
            // turn the continue into a GOTO
            *proto_intruction = ProtoInstruction::Goto(last_loop_start);
        }
        // solve break as Goto labels
        // must iters the label in reverse
        // relies on the fact that a Endloop label must be located *AFTER* 
        // the *break* expression
        let mut last_loop_end: usize = 0;
        for proto_intruction in proto_instructions.iter_mut().rev() {
            match *proto_intruction {
                ProtoInstruction::AddrLabel( Addr { addr, kind: AddrKind::EndLoop }) => {
                    // store the instruction index 
                    last_loop_end = addr;
                    continue;
                },
                ProtoInstruction::Break => {},
                _ => continue,
            }
            // turn the continue into a GOTO
            *proto_intruction = ProtoInstruction::Goto(last_loop_end);
        }

        // solve labels as instruction offset
        let mut label_to_offset: HashMap<usize, usize> = HashMap::new();
        let mut label_count: usize = 0;
        // collect labels
        for (i, proto_intruction) in proto_instructions.iter().enumerate() {
            if let ProtoInstruction::AddrLabel( Addr { addr, .. }) = *proto_intruction {
                // store the instruction index 
                label_to_offset.insert(addr, i - label_count);
                label_count += 1;
            }
        }

        // build the actual instructions
        let mut instructions: Vec<Instruction> = Vec::new();
        let final_idx = proto_instructions.len() -1;
        for (i, proto_instruction) in proto_instructions.into_iter().enumerate() {
            match proto_instruction {
                ProtoInstruction::Goto(label) => {
                    let offset = label_to_offset.get(&label)?;
                    instructions.push(Goto(*offset));
                },
                ProtoInstruction::GotoIf(label) => {
                    let offset = label_to_offset.get(&label)?;
                    instructions.push(GotoIf(*offset));
                },
                ProtoInstruction::AddrLabel(..) => {},
                ProtoInstruction::NewRef(s) => instructions.push(NewRef(s)),
                ProtoInstruction::Load(s) => instructions.push(Load(s)),
                ProtoInstruction::Store(s) => instructions.push(Store(s)),
                ProtoInstruction::Val(proto_val) => {
                    use Value::*;
                    let val = match proto_val {
                        ProtoValue::Str(s) => Str(s),
                        ProtoValue::Bool(b) => Bool(b),
                        ProtoValue::Num(n) => Num(n),
                        ProtoValue::Function(label) => {
                            let offset = label_to_offset.get(&label)?;
                            Function(FnKind::Address(*offset))
                        },
                        ProtoValue::Null => Null,
                        ProtoValue::Undefined => Undefined,
                    };
                    instructions.push(Val(val));
                }
                ProtoInstruction::FnCall => instructions.push(FnCall),
                ProtoInstruction::FnRet => instructions.push(FnRet),
                ProtoInstruction::PushToNext(n) => instructions.push(PushToNext(n)),
                ProtoInstruction::NewStack => instructions.push(NewStack),
                ProtoInstruction::ClearStack => instructions.push(ClearStack),
                ProtoInstruction::DelStack => {
                    if i != final_idx { 
                        // don't delete the root stack, we will use it to evaluate the
                        // return value of a whole script
                        instructions.push(DelStack);
                    }
                },
                // binary op
                ProtoInstruction::Add => instructions.push(Add),
                ProtoInstruction::Div => instructions.push(Div),
                ProtoInstruction::Sub => instructions.push(Sub),
                ProtoInstruction::Mul => instructions.push(Mul),
                ProtoInstruction::Pow => instructions.push(Pow),
                ProtoInstruction::Mod => instructions.push(Mod),
                ProtoInstruction::Equal => instructions.push(Equal),
                ProtoInstruction::NotEqual => instructions.push(NotEqual),
                ProtoInstruction::GreaterThan => instructions.push(GreaterThan),
                ProtoInstruction::GreaterThanOrEqual => instructions.push(GreaterThanOrEqual),
                ProtoInstruction::LessThan => instructions.push(LessThan),
                ProtoInstruction::LessThanOrEqual => instructions.push(LessThanOrEqual),
                ProtoInstruction::And => instructions.push(And),
                ProtoInstruction::Or => instructions.push(Or),
                // unary op
                ProtoInstruction::Minus => instructions.push(Minus),
                ProtoInstruction::Plus => instructions.push(Plus),
                ProtoInstruction::Not => instructions.push(Not),
                ProtoInstruction::NewLoopCtx => instructions.push(NewLoopCtx),
                ProtoInstruction::DelLoopCtx => instructions.push(DelLoopCtx),
                ProtoInstruction::PopToLoopCtx => instructions.push(PopToLoopCtx),
                // both must have been replaced by Goto
                ProtoInstruction::Break | ProtoInstruction::Continue => {},
            }
        }
        Some(instructions)
    }

    /// Compile an AST into an instruction set
    pub fn compile(ast: &Expr) -> Option<Vec<Instruction>> {
        // compile into proto instructions
        let proto_instructions = Self::preprocess(ast)?;
        // solve labels into addresses
        Self::solve_labels(proto_instructions)
        
    }
}
