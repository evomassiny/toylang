use std::collections::HashMap;
use crate::compiler::{GLOBAL_SCOPE_LABEL};
use crate::builtins::{FnKind,NativeFn,Value,LexicalLabel,ContextID};


#[derive(Debug)]
pub struct ContextError {
    pub msg: String,
}
impl std::error::Error for ContextError {}
impl std::fmt::Display for ContextError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg,)
    }
}
macro_rules! context_error {
    ($msg:expr) => {
        ContextError {
            msg: format!("Execution error: {}", $msg),
        }
    };
}

/// This enum identities 2 sets of stacks
#[derive(Debug)]
pub enum StackKind {
    Block,
    Loop,
}

#[derive(Debug)]
pub struct ValueStack {
    values: Vec<Value>,
    kind: StackKind,
}
impl ValueStack {
    fn new(kind: StackKind) -> Self {
        Self {
            values: Vec::new(),
            kind: kind,
        }
    }
}

#[derive(Debug)]
pub struct ExecutionCtx {
    /// The variable bindings of the current ExecutionCtx
    bindings: HashMap<String, Value>,
    /// a label identifiying the lexical stack of `self`,
    /// only used for debugging purpose
    label: LexicalLabel,
    /// this ContextID of the parent context, or None if `self` is the global context
    parent: Option<ContextID>,
    /// the ID of `self`, act as a Key in the Context.store
    this: ContextID,
    /// a stacks of `ValueStack`, to store temporary 
    /// results
    stacks: Vec<ValueStack>,
}

impl ExecutionCtx {

    /// Build the gloabl stack, include all the buit-in functions
    fn new_global() -> Self {
        Self {
            stacks: vec![ValueStack::new(StackKind::Block)], 
            bindings: HashMap::new(),
            label: GLOBAL_SCOPE_LABEL.into(),
            parent: None,
            this: 0,
        }
    }
}


/// A `Context` defines a set of stacks,
/// reflecting the state of a Byte code interpreter based on stack machine.
/// It is itself a stack of stacks.
///
/// A stack represents 2 things:
/// * the references (bindings) and their associated values
/// * a register stack (if that make sense...) which is just a storage
/// for the temporary results of each expressions
///
/// A `Context` allows an Executor to keep track of its memory
/// during the instructions evalutation.
pub struct Context {
    pub context_store: HashMap<ContextID, ExecutionCtx>,
    // used to generate uniq ExecutionCtx labels
    context_counter: usize,
    /// store the execution context stack
    context_stack: Vec<ContextID>,
}

impl Context {

    /// A new context contains:
    /// * the global execution context
    /// * an empty local stack
    pub fn new() -> Self {
        let mut store = HashMap::new();
        store.insert(0, ExecutionCtx::new_global());
        let ctx = Self { 
            context_store: store,
            context_counter: 1,
            context_stack: vec![0,],
        };
        ctx
    }

    /// Create a new execution context
    /// and gives it a uniq ContextID;
    /// then push it to the stack,
    /// and finally
    pub fn new_context(&mut self, label: &LexicalLabel) {
        let ctx_id = self.context_counter;
        self.context_counter = self.context_counter + 1;

        let parent_id = match self.context_stack.last() {
            Some(parent_id) => Some(*parent_id),
            None => None,
        };
        self.context_store.insert(
            ctx_id,
            ExecutionCtx {
                bindings: HashMap::new(),
                parent: parent_id,
                label: label.clone(),
                this: ctx_id,
                stacks: vec![ValueStack::new(StackKind::Block)], 
            }
        );
        self.context_stack.push(ctx_id);
    }

    /// Add a native function to the global context
    pub fn add_native_function(&mut self, name: &str, func: NativeFn,) -> Result<(), ContextError> {
        // get the global stack ID
        match self.context_stack.first() {
            Some(this) => {
                self.context_store.get_mut(this)
                    .ok_or(context_error!("stacks stack is empty"))?
                    .bindings
                    .insert(name.to_string(), Value::Function(FnKind::Native(func), this.clone()));
                return Ok(());
            },
            None => return Err(context_error!("No global stack"))
        }
    }


    /// Creates a new reference and store it into the current context
    pub fn create_ref(&mut self, name: &str) -> Result<(), ContextError> {
        let ctx = self.context_stack.last()
            .ok_or(context_error!("context stack is empty"))?;
        self.context_store.get_mut(ctx)
            .ok_or(context_error!("unknown context id"))?
            .bindings
            .insert(name.to_string(), Value::Undefined);
        Ok(())
    }

    pub fn store(&mut self, name: &str, val: Value) -> Result<(), ContextError> {
        let mut ctx = self.current_context()?;
        // crawl back the nested execution context_store until we found the
        // reference to mutate
        loop {
            match self.context_store.get_mut(&ctx) {
                Some(context) => {
                    if let Some(stored) = context.bindings.get_mut(name) {
                        *stored = val;
                        return Ok(());
                    }
                    // otherwise lookup in an 'higher' stack
                    match &context.parent {
                        Some(id) => ctx = *id,
                        None => return Err(
                            context_error!(format!("assignment error: Unknown reference {} (in {})", name, ctx))
                        ),
                    } 
                },
                None => return Err(
                    context_error!(format!("assignment error: Unknown reference {} (in {})", name, ctx))
                ),
            }
        }
    }

    pub fn load(&self, name: &str) -> Result<Value, ContextError> {
        let mut ctx = self.current_context()?;
        while let Some(context) = self.context_store.get(&ctx) {
            if let Some(val) = context.bindings.get(name) {
                return Ok(val.clone());
            }
            match &context.parent {
                Some(id) => ctx = *id,
                None => break,
            } 
        }
        Err(context_error!(format!("load error: Unknown reference {} (in {})", name, ctx)))
    }

    /// Change context (for instance in function call)
    pub fn push_context(&mut self, ctx_id: ContextID) {
        self.context_stack.push(ctx_id);
    }

    /// Change context (for instance after function call)
    pub fn pop_context(&mut self) {
        let _ = self.context_stack.pop();
    }

    /// return the current context ID
    pub fn current_context(&self) -> Result<ContextID, ContextError> {
        match self.context_stack.last() {
            Some(c) => Ok(*c),
            None => return Err(context_error!("context stack is empty")),
        }
    }

    /// add a block stack to the local context
    pub fn add_block_stack(&mut self) -> Result<(), ContextError> {
        let ctx_id = self.current_context()?;
        match self.context_store.get_mut(&ctx_id) {
            Some(ctx) => ctx.stacks.push(ValueStack::new(StackKind::Block)),
            None => return Err(context_error!("Cannot add block stack: Context not in store.")),
        }
        Ok(())
    }

    /// add a loop stack to the local context
    pub fn add_loop_stack(&mut self) -> Result<(), ContextError> {
        let ctx_id = self.current_context()?;
        match self.context_store.get_mut(&ctx_id) {
            Some(ctx) => ctx.stacks.push(ValueStack::new(StackKind::Loop)),
            None => return Err(context_error!("Cannot add loop stack: Context not in store.")),
        }
        Ok(())
    }

    /// pop stacks until we poped one Loop Scope
    pub fn pop_loop_stack(&mut self) -> Result<(), ContextError> {
        let ctx_id = self.current_context()?;
        match self.context_store.get_mut(&ctx_id) {
            Some(ctx) => {
                while let Some(ValueStack { kind, .. }) = ctx.stacks.pop() {
                    match kind {
                        StackKind::Loop => break,
                        _ => {}
                    }
                }
            },
            None => return Err(context_error!("Cannot pop loop stack: Context not in store.")),
        }
        Ok(())
    }

    /// pop stacks until we reach a Loop Scope (preserve it in place)
    pub fn pop_until_loop_stack(&mut self) -> Result<(), ContextError> {
        let ctx_id = self.current_context()?;
        match self.context_store.get_mut(&ctx_id) {
            Some(ctx) => {
                while let Some(ValueStack { kind, .. }) = ctx.stacks.last() {
                    match kind {
                        StackKind::Loop => break,
                        _ => {
                            let _ = ctx.stacks.pop();
                        }
                    }
                }
            },
            None => return Err(context_error!("Cannot add loop stack: Context not in store.")),
        }
        Ok(())
    }

    /// pop a stack, no matter its kind
    pub fn pop_stack(&mut self) -> Option<ValueStack> {
        let ctx_id = self.context_stack.last()?;
        let ctx = self.context_store.get_mut(ctx_id)?;
        ctx.stacks.pop()
    }

    /// push a value to the current stack of the current context
    pub fn push_value(&mut self, val: Value) -> Result<(), ContextError> {
        let ctx_id = self.current_context()?;
        let ctx = self.context_store.get_mut(&ctx_id)
            .ok_or(context_error!("cannot push value to context stack, context not in store"))?;
        match ctx.stacks.last_mut() {
            Some(ref mut stack) => stack.values.push(val),
            None => return Err(context_error!("Cannot push value to stack, not stack in context.")),
        }
        Ok(())
    }

    /// pop a value to the register stack
    pub fn pop_value(&mut self) -> Option<Value> {
        let ctx_id = self.context_stack.last()?;
        let ctx = self.context_store.get_mut(ctx_id)?;
        ctx.stacks.last_mut()?.values.pop()
    }

    /// flush the register stack
    pub fn flush_values(&mut self) -> Result<(), ContextError> {
        let ctx_id = self.current_context()?;
        let ctx = self.context_store.get_mut(&ctx_id)
            .ok_or(context_error!("cannot push value to context stack, context not in store"))?;
        match ctx.stacks.last_mut() {
            Some(ref mut stack) => stack.values.clear(),
            None => return Err(context_error!("no stack to flush in current context"))
        }
        Ok(())
    }

}
