use crate::builtins::{FnKind,NativeFn,Value};
use std::collections::HashMap;
use crate::compiler::{ContextLabel,GLOBAL_SCOPE_LABEL};


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

struct LexicalContext {
    bindings: HashMap<String, Value>,
    parent: Option<ContextLabel>,
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
    contexts: HashMap<ContextLabel, LexicalContext>
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Self { 
            local_scopes: vec![], 
            contexts: HashMap::new(),
        };
        //ctx.add_block_scope();
        ctx
    }

    pub fn add_block_scope(&mut self) {
        self.local_scopes.push(LocalScope::new(LocalScopeKind::Block));
    }

    pub fn add_loop_scope(&mut self) {
        self.local_scopes.push(LocalScope::new(LocalScopeKind::Loop));
    }

    pub fn new_context(&mut self, ctx: &ContextLabel, parent_ctx: &Option<ContextLabel>) {
        self.contexts.insert(
            ctx.clone(),
            LexicalContext {
                bindings: HashMap::new(),
                parent: parent_ctx.clone(),
            }
        );
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

    pub fn add_native_function(
        &mut self,
        name: &str,
        ctx: &ContextLabel,
        func: NativeFn,
    ) -> Result<(), ContextError> {
        self.contexts.get_mut(ctx)
            .ok_or(context_error!("scopes stack is empty"))?
            .bindings
            .insert(name.to_string(), Value::Function(FnKind::Native(func)));
        Ok(())

    }

    pub fn create_ref(&mut self, name: &str, ctx: &ContextLabel) -> Result<(), ContextError> {
        self.contexts.get_mut(ctx)
            .ok_or(context_error!("scopes stack is empty"))?
            .bindings
            .insert(name.to_string(), Value::Undefined);
        Ok(())
    }

    pub fn store(&mut self, name: &str, ctx: &ContextLabel, val: Value) -> Result<(), ContextError> {
        if let Some(stored) = self.contexts.get_mut(ctx)
                .ok_or(context_error!("scopes stack is empty"))?
                .bindings
                .get_mut(name) {
            *stored = val;
            return Ok(());
        }
        Err(context_error!(format!("Unknown reference {}", name)))
    }

    pub fn load(&self, name: &str, ctx: &ContextLabel) -> Result<Value, ContextError> {
        let mut ctx = ctx;
        while let Some(context) = self.contexts.get(ctx) {
            if let Some(val) = context.bindings.get(name) {
                return Ok(val.clone());
            }
            match &context.parent {
                Some(label) => ctx = &label,
                None => break,
            } 
        }
        Err(context_error!(format!("Unknown reference {}", name)))
    }
}
