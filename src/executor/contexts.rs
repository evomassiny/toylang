use crate::builtins::{FnKind,NativeFn,Value};
use std::collections::HashMap;

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

pub enum ScopeKind {
    Function,
    Block,
    Loop,
}

pub struct Scope {
    values: Vec<Value>,
    bindings: HashMap<String, Value>,
    kind: ScopeKind,
}
impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            values: Vec::new(),
            bindings: HashMap::new(),
            kind: kind,
        }
    }
}

/// A `Context` defines a set of scopes,
/// reflecting the state of an interpreter based on stack machine.
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
    scopes: Vec<Scope>,
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Self { scopes: vec![] };
        ctx.add_block_scope();
        ctx
    }

    pub fn add_block_scope(&mut self) {
        self.scopes.push(Scope::new(ScopeKind::Block));
    }

    pub fn add_loop_scope(&mut self) {
        self.scopes.push(Scope::new(ScopeKind::Loop));
    }

    pub fn add_function_scope(&mut self) {
        self.scopes.push(Scope::new(ScopeKind::Function));
    }

    /// pop scopes until we poped one Function Scope
    pub fn pop_function_scope(&mut self) {
        while let Some(Scope { kind, .. }) = self.scopes.pop() {
            match kind {
                ScopeKind::Function => break,
                _ => {}
            }
        }
    }

    /// pop scopes until we poped one Loop Scope
    pub fn pop_loop_scope(&mut self) {
        while let Some(Scope { kind, .. }) = self.scopes.pop() {
            match kind {
                ScopeKind::Loop => break,
                _ => {}
            }
        }
    }

    /// pop scopes until we reach a Loop Scope (preserve it in place)
    pub fn pop_until_loop_scope(&mut self) {
        while let Some(Scope { kind, .. }) = self.scopes.last() {
            match kind {
                ScopeKind::Loop => break,
                _ => {
                    let _ = self.scopes.pop();
                }
            }
        }
    }

    /// pop a scope, no matter its kind
    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    /// push a value to the register stack
    pub fn push_value(&mut self, val: Value) {
        match self.scopes.last_mut() {
            Some(ref mut scope) => scope.values.push(val),
            None => {
                let mut scope = Scope::new(ScopeKind::Block);
                scope.values.push(val);
            }
        }
    }

    /// pop a value to the register stack
    pub fn pop_value(&mut self) -> Option<Value> {
        self.scopes.last_mut()?.values.pop()
    }

    /// flush the register stack
    pub fn flush_values(&mut self) {
        match self.scopes.last_mut() {
            Some(ref mut scope) => scope.values.clear(),
            None => self.add_block_scope(),
        }
    }

    pub fn add_native_function(
        &mut self,
        name: &str,
        func: NativeFn,
    ) -> Result<(), ContextError> {
        self.scopes
            .last_mut()
            .ok_or(context_error!("scopes stack is empty"))?
            .bindings
            .insert(name.to_string(), Value::Function(FnKind::Native(func)));
        Ok(())

    }

    pub fn create_ref(&mut self, name: &str) -> Result<(), ContextError> {
        self.scopes
            .last_mut()
            .ok_or(context_error!("scopes stack is empty"))?
            .bindings
            .insert(name.to_string(), Value::Undefined);
        Ok(())
    }

    pub fn store(&mut self, name: &str, val: Value) -> Result<(), ContextError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(stored) = scope.bindings.get_mut(name) {
                *stored = val;
                return Ok(());
            }
        }
        Err(context_error!(format!("Unknown reference {}", name)))
    }

    pub fn load(&self, name: &str) -> Result<Value, ContextError> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.bindings.get(name) {
                return Ok(val.clone());
            }
        }
        Err(context_error!(format!("Unknown reference {}", name)))
    }
}
