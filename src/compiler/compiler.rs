use std::collections::HashMap;
use crate::ast::Expr;
use crate::compiler::preprocessors::{PreValue,PreInstruction,LabelGenerator,Addr,AddrKind};
use crate::compiler::preprocessors as pp;
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

    /// compile an AST into a vector of PreInstruction
    /// an intermediate representation of the actual Instruction set
    /// with unsolved address (labels)
    pub fn preprocess(expression: &Expr) -> Option<Vec<PreInstruction>> {
        let mut instructions: Vec<Vec<PreInstruction>> = Vec::new();
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
            let insts: Vec<PreInstruction> = match expression {
                Const(c) => pp::preprocess_const(c)?,
                Return(_) => pp::preprocess_return(sub_instructions)?,
                If(..) => pp::preprocess_if(sub_instructions, &mut labels)?,
                Block(..) => pp::preprocess_block(sub_instructions)?,
                LetDecl(name, _) => pp::preprocess_let(name, sub_instructions)?,
                Local(id) => pp::preprocess_local(id)?,
                Assign(id, ..) => pp::assign_to_instructions(id, sub_instructions)?,
                Call(..) => pp::preprocess_call(sub_instructions)?,
                FunctionDecl(name, args, _block) => 
                    pp::preprocess_function_decl(sub_instructions, name, args, &mut labels)?,
                BinaryOp(op, ..) => 
                    pp::preprocess_binary_op(op, sub_instructions, &mut labels)?,
                UnaryOp(op, ..) => pp::preprocess_unary_op(op, sub_instructions)?,
                WhileLoop(..) => pp::preprocess_while(sub_instructions, &mut labels)?,
                Break => pp::preprocess_break()?,
                Continue => pp::preprocess_continue()?,
            };
            // Push those expression to the 
            if insts.len() > 0 {
                instructions.push(insts);
            }

        }
        Some(instructions.pop()?)
    }

    /// Compile an AST into an instruction set
    pub fn compile(ast: &Expr) -> Option<Vec<Instruction>> {
        use Instruction::*;
        // compile into proto instructions
        let mut pre_instructions = Self::preprocess(ast)?;
        
        // solve 'continue' as Goto labels
        // relies on the fact that a Beginloop label must be located *BEFORE* 
        // the *continue* expression
        let mut last_loop_start: usize = 0;
        for pre_intruction in pre_instructions.iter_mut() {
            match *pre_intruction {
                PreInstruction::AddrLabel( Addr { addr, kind: AddrKind::BeginLoop }) => {
                    // store the instruction index 
                    last_loop_start = addr;
                    continue;
                },
                PreInstruction::Continue => {},
                _ => continue,
            }
            // turn the continue into a GOTO
            *pre_intruction = PreInstruction::Goto(last_loop_start);
        }
        // solve break as Goto labels
        // must iters the label in reverse
        // relies on the fact that a Endloop label must be located *AFTER* 
        // the *break* expression
        let mut last_loop_end: usize = 0;
        for pre_intruction in pre_instructions.iter_mut().rev() {
            match *pre_intruction {
                PreInstruction::AddrLabel( Addr { addr, kind: AddrKind::EndLoop }) => {
                    // store the instruction index 
                    last_loop_end = addr;
                    continue;
                },
                PreInstruction::Break => {},
                _ => continue,
            }
            // turn the continue into a GOTO
            *pre_intruction = PreInstruction::Goto(last_loop_end);
        }

        // solve labels as instruction offset
        let mut label_to_offset: HashMap<usize, usize> = HashMap::new();
        let mut label_count: usize = 0;
        // collect labels
        for (i, pre_intruction) in pre_instructions.iter().enumerate() {
            if let PreInstruction::AddrLabel( Addr { addr, .. }) = *pre_intruction {
                // store the instruction index 
                label_to_offset.insert(addr, i - label_count);
                label_count += 1;
            }
        }

        // build the actual instructions
        let mut instructions: Vec<Instruction> = Vec::new();
        let final_idx = pre_instructions.len() -1;
        for (i, pre_instruction) in pre_instructions.into_iter().enumerate() {
            match pre_instruction {
                PreInstruction::Goto(label) => {
                    let offset = label_to_offset.get(&label)?;
                    instructions.push(Goto(*offset));
                },
                PreInstruction::GotoIf(label) => {
                    let offset = label_to_offset.get(&label)?;
                    instructions.push(GotoIf(*offset));
                },
                PreInstruction::AddrLabel(..) => {},
                PreInstruction::NewRef(s) => instructions.push(NewRef(s)),
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
                            Function(FnKind::Address(*offset))
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
                PreInstruction::DelStack => {
                    if i != final_idx { 
                        // don't delete the root stack, we will use it to evaluate the
                        // return value of a whole script
                        instructions.push(DelStack);
                    }
                },
                // binary op
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
                // unary op
                PreInstruction::Minus => instructions.push(Minus),
                PreInstruction::Plus => instructions.push(Plus),
                PreInstruction::Not => instructions.push(Not),
                PreInstruction::NewLoopCtx => instructions.push(NewLoopCtx),
                PreInstruction::DelLoopCtx => instructions.push(DelLoopCtx),
                PreInstruction::PopToLoopCtx => instructions.push(PopToLoopCtx),
                // both must have been replaced by Goto
                PreInstruction::Break | PreInstruction::Continue => {},
            }
        }
        Some(instructions)
    }
}
