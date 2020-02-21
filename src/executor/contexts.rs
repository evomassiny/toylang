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

/// This enum identities 2 sets of scopes
pub enum LocalScopeKind {
    Block,
    Loop,
}

pub struct LocalScope {
    values: Vec<Value>,
    kind: LocalScopeKind,
}
impl LocalScope {
    fn new(kind: LocalScopeKind) -> Self {
        Self {
            values: Vec::new(),
            kind: kind,
        }
    }
}

struct ExecutionCtx {
    bindings: HashMap<String, Value>,
    label: LexicalLabel,
    parent: Option<ContextID>,
    this: ContextID,
}


/// A `Context` defines a set of scopes,
/// reflecting the state of a Byte code interpreter based on stack machine.
/// It is itself a stack of scopes.
///
/// A scope represents 2 things:
/// * the references (bindings) and their associated values
/// * a register stack (if that make sense...) which is just a storage
/// for the temporary results of each expressions
///
/// A `Context` allows an Executor to keep track of its memory
/// during the instructions evalutation.
pub struct Context {
    local_scopes: Vec<LocalScope>,
    contexts: HashMap<ContextID, ExecutionCtx>,
    // used to generate uniq ExecutionCtx labels
    context_counter: usize,
    /// store the execution context stack
    context_stack: Vec<ContextID>,
}

impl Context {
    pub fn new() -> Self {
        let ctx = Self { 
            local_scopes: vec![], 
            contexts: HashMap::new(),
            context_counter: 0,
            context_stack: vec![],
        };
        ctx
    }

    /// Create a new execution context
    /// and gives it a uniq ContextID;
    /// finally: push it to the stack
    pub fn new_context(&mut self, label: &LexicalLabel) {
        let parent_ctx: Option<ContextID> = match self.context_stack.last() {
            Some(c) => Some(*c),
            None => None,
        };
        let ctx_id = self.context_counter + 1;
        self.context_counter = ctx_id;
        self.contexts.insert(
            ctx_id,
            ExecutionCtx {
                bindings: HashMap::new(),
                parent: parent_ctx,
                label: label.clone(),
                this: ctx_id,
            }
        );
        self.context_stack.push(ctx_id);
    }

    pub fn add_native_function(
        &mut self,
        name: &str,
        lex_label: &LexicalLabel,
        func: NativeFn,
    ) -> Result<(), ContextError> {
        // lookup the context store to find one that match 
        // `label`
        let mut ctx_id: Option<ContextID> = None;
        for ExecutionCtx{ label, this, ..} in self.contexts.values() {
            if *lex_label == *label {
                ctx_id = Some(this.clone());
                break;
            }

        }
        match ctx_id {
            Some(this) => {
                self.contexts.get_mut(&this)
                    .ok_or(context_error!("scopes stack is empty"))?
                    .bindings
                    .insert(name.to_string(), Value::Function(FnKind::Native(func), this.clone()));
                return Ok(());
            },
            None => return Err(context_error!(format!("Unknown context {}", lex_label)))
        }
    }

    /// Creates a new reference and store it into the current context
    pub fn create_ref(&mut self, name: &str) -> Result<(), ContextError> {
        let ctx = self.context_stack.last()
            .ok_or(context_error!("context stack is empty"))?;
        self.contexts.get_mut(ctx)
            .ok_or(context_error!("unknown context id"))?
            .bindings
            .insert(name.to_string(), Value::Undefined);
        Ok(())
    }

    pub fn store(&mut self, name: &str, val: Value) -> Result<(), ContextError> {
        let mut ctx = self.current_context()?;
        // crawl back the nested execution contexts until we found the
        // reference to mutate
        loop {
            match self.contexts.get_mut(&ctx) {
                Some(context) => {
                    if let Some(stored) = context.bindings.get_mut(name) {
                        *stored = val;
                        return Ok(());
                    }
                    // otherwise lookup in an 'higher' scope
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
        while let Some(context) = self.contexts.get(&ctx) {
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
            None => Err(context_error!("context stack is empty")),
        }
    }

    pub fn add_block_scope(&mut self) {
        self.local_scopes.push(LocalScope::new(LocalScopeKind::Block));
    }

    pub fn add_loop_scope(&mut self) {
        self.local_scopes.push(LocalScope::new(LocalScopeKind::Loop));
    }

    /// pop scopes until we poped one Loop Scope
    pub fn pop_loop_scope(&mut self) {
        while let Some(LocalScope { kind, .. }) = self.local_scopes.pop() {
            match kind {
                LocalScopeKind::Loop => break,
                _ => {}
            }
        }
    }

    /// pop scopes until we reach a Loop Scope (preserve it in place)
    pub fn pop_until_loop_scope(&mut self) {
        while let Some(LocalScope { kind, .. }) = self.local_scopes.last() {
            match kind {
                LocalScopeKind::Loop => break,
                _ => {
                    let _ = self.local_scopes.pop();
                }
            }
        }
    }

    /// pop a scope, no matter its kind
    pub fn pop_scope(&mut self) -> Option<LocalScope> {
        self.local_scopes.pop()
    }

    /// push a value to the register stack
    pub fn push_value(&mut self, val: Value) {
        match self.local_scopes.last_mut() {
            Some(ref mut scope) => scope.values.push(val),
            None => {
                let mut scope = LocalScope::new(LocalScopeKind::Block);
                scope.values.push(val);
            }
        }
    }

    /// pop a value to the register stack
    pub fn pop_value(&mut self) -> Option<Value> {
        self.local_scopes.last_mut()?.values.pop()
    }

    /// flush the register stack
    pub fn flush_values(&mut self) {
        match self.local_scopes.last_mut() {
            Some(ref mut scope) => scope.values.clear(),
            None => self.add_block_scope(),
        }
    }

}
