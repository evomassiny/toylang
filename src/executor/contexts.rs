use std::collections::{HashMap,HashSet};
use std::iter::FromIterator;
use crate::builtins::{FnKind,NativeFn,Value,ContextID};


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

/// Tells the `Context` to run garbage collections
/// every `CREATION_NB_UNTIL_GC` ExecutionCtx creations
const CREATION_NB_UNTIL_GC: usize = 500;

#[derive(Debug)]
pub struct ExecutionCtx {
    /// The variable bindings of the current ExecutionCtx
    bindings: HashMap<String, Value>,
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
            parent: None,
            this: 0,
        }
    }
}


/// A `Context` defines the memory of a stack-based Byte code interpreter.
/// 
/// A context contains a store of simily-stackframe called `ExecutionCtx`,
/// every function call create a new instance of `ExecutionCtx` which is stored
/// into the `context_store`.
/// Each `ExecutionCtx` hold the variables defined during a function call, 
/// and the ContextId of the context that created it.
/// One can follow those ID links to solve any variable bindings.
///
/// Note: Function Objects are binded to ContextID, 
/// which allows one to solve a variable bindings related to their `ExecutionCtx.parent`.
///
/// The current context stack is stored into a Vec called `context_stack`,
/// it helps managing context accross function calls.
pub struct Context {
    pub context_store: HashMap<ContextID, ExecutionCtx>,
    // used to generate uniq ExecutionCtx labels
    context_counter: usize,
    /// store the execution context stack
    context_stack: Vec<ContextID>,
    /// context created since garbage_collection
    context_created_since_gc: usize,
}

impl Context {

    /// A new context contains:
    /// * the global execution context
    /// * an empty local stack
    pub fn new() -> Self {
        let mut store = HashMap::with_capacity(CREATION_NB_UNTIL_GC);
        store.insert(0, ExecutionCtx::new_global());
        Self { 
            context_store: store,
            context_counter: 1,
            context_stack: vec![0,],
            context_created_since_gc: 1,
        }
    }

    /// Create a new execution context
    /// and gives it a uniq ContextID;
    /// then push it to the stack.
    /// Bind it to `parent_ctx_id`.
    ///
    /// Returns the context ID created.
    pub fn add_binded_context(&mut self, parent_ctx_id: ContextID) -> ContextID {
        // run GC if needed
        self.context_created_since_gc += 1;
        if self.context_created_since_gc == CREATION_NB_UNTIL_GC {
            let _ = self.collect_garbage();
            self.context_created_since_gc = 0;
        };
        // create the new context
        let ctx_id = self.context_counter;
        self.context_counter = self.context_counter + 1;
        self.context_store.insert(
            ctx_id,
            ExecutionCtx {
                bindings: HashMap::new(),
                parent: Some(parent_ctx_id),
                this: ctx_id,
                stacks: vec![ValueStack::new(StackKind::Block)], 
            }
        );
        self.context_stack.push(ctx_id);
        return ctx_id;
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

    /// add a loop stack to the current context
    pub fn add_loop_stack(&mut self) -> Result<(), ContextError> {
        let ctx_id = self.current_context()?;
        match self.context_store.get_mut(&ctx_id) {
            Some(ctx) => {
                ctx.stacks.push(ValueStack::new(StackKind::Loop));
            },
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

    /// Run tracing garbage collections:
    /// Crawl the Execution context stack to find unreferenced `ExecutionCtx`, then delete them.
    pub fn collect_garbage(&mut self) -> Result<(), ContextError> {
        let mut to_crawl: Vec<ContextID> = self.context_stack.clone();
        let mut whitelist: HashSet<ContextID> = HashSet::new();
        let mut crawled: HashSet<ContextID> = HashSet::new();
        while let Some(ctx_id) = to_crawl.pop() {
            // keep the referenced Contexts
            whitelist.insert(ctx_id.clone());
            // fetch the context
            let ctx = self.context_store.get(&ctx_id)
                .ok_or(context_error!(format!("context ID {} is unreferenced", &ctx_id)))?;
            // add the parent to the crawling queue
            if let Some(parent_ctx_id) = ctx.parent {
                if !crawled.contains(&parent_ctx_id) {
                    to_crawl.push(parent_ctx_id);
                }
            }
            // iter the Context bindings and crawl the context they're refering to
            for value in ctx.bindings.values() {
                match value {
                    Value::Function(_kind, fn_ctx) => {
                        if !crawled.contains(fn_ctx) {
                            to_crawl.push(*fn_ctx);
                        }
                    },
                    _ => continue,
                }
            }
            // add the current context to the crawled set, so we don't crawl it twice
            crawled.insert(ctx_id.clone());
        }

        // remove the unreferenced context
        let all_contexts: HashSet<ContextID> = HashSet::from_iter(self.context_store.keys().cloned());
        for ctx_id in all_contexts.difference(&whitelist) {
            self.context_store.remove(&ctx_id);
        }
        Ok(())
    }

}

#[cfg(test)]
mod tests {
    use crate::executor::{Context,ExecutionCtx};
    use crate::builtins::{Value,FnKind,ContextID};
    use Value::*;

    #[test]
    fn test_garbage_collections() {
        let mut ctx = Context::new();
        let global_ctx: ContextID = *ctx.context_stack.last().unwrap();
        // create a child context
        let child = ctx.add_binded_context(global_ctx);
        ctx.pop_context();
        // and reference it in the global context
        let _ = ctx.create_ref("covfefe");
        let _ = ctx.store("covfefe", Value::Function(FnKind::Address(1), child));
        // create 2 unreferenced contexts
        let unref_ctx = ctx.add_binded_context(child);
        ctx.pop_context();
        let unref_but_running = ctx.add_binded_context(global_ctx);
        // run the GC
        let _ = ctx.collect_garbage();
        // `global_ctx` and `child` only should be kept
        assert_eq!(ctx.context_store.contains_key(&global_ctx), true, "Global scope was removed");
        assert_eq!(ctx.context_store.contains_key(&child), true, "referenced scope was removed");
        assert_eq!(ctx.context_store.contains_key(&unref_ctx), false, "unreferenced scope was not removed");
        assert_eq!(ctx.context_store.contains_key(&unref_but_running), true, "active/runnnig scope was removed");
    }
}
