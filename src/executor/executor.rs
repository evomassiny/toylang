use std::collections::HashMap;
use crate::ast::Expr;
use crate::compiler::{Compiler,Instruction};
use crate::builtins::Value;

#[derive(Debug)]
pub struct ExecutionError {
    msg: String    
}
impl std::error::Error for ExecutionError {}
impl std::fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg,)
    }
}
macro_rules! exec_error {
    ($msg:expr) => {
        ExecutionError { msg: format!("Execution error: {}", $msg) }
    }
}

pub enum ExecStatus {
    Running, 
    Finished,
}

pub struct Executor<'inst> {
    pub instructions: &'inst [Instruction],
    value_stacks: Vec<Vec<Value>>,
    passthrough: Vec<Value>,
    execution_pointers: Vec<usize>,
    bindings: Vec<HashMap<String, Value>>,
}

impl <'inst> Executor <'inst> { 
    pub fn from_instructions(instructions: &'inst [Instruction]) -> Self {
        Self { 
            instructions: instructions,
            value_stacks: Vec::new(),
            passthrough: Vec::new(),
            execution_pointers: vec![0],
            bindings: vec![HashMap::new()],
        }
    }

    /// load the value referenced by `name`
    fn load(&self, name: &str) -> Value {
        for binds in self.bindings.iter().rev() {
            if let Some(v) = binds.get(name) {
                return v.clone();
            }
        }
        Value::Undefined
    }
    
    /// store a value at the referenced place
    /// If the reference doesn't points to anything, returns an error
    fn store(&mut self, name: &str, val: Value) -> Result<(), ExecutionError> {
        for binds in self.bindings.iter_mut().rev() {
            if let Some(v) = binds.get_mut(name) {
                *v = val;
                return Ok(());
            }
        }
        Err(exec_error!(format!("unknown reference {}", name)))
    }
    
    /// create a reference
    /// If the reference doesn't points to anything, returns an error
    fn create_ref(&mut self, name: &str) -> Result<(), ExecutionError> {
        self.bindings.last_mut()
            .ok_or(exec_error!("Empty bindings stack"))?
            .insert(name.to_string(), Value::Undefined);
        Ok(())
    }

    /// increments the execution pointer
    fn goto_next(&mut self) {
        if let Some(pc) = self.execution_pointers.last_mut() {
            *pc += 1;
        }
    }

    /// set the instruction pointer to address
    fn goto(&mut self, address: usize) {
        if let Some(pc) = self.execution_pointers.last_mut() {
            *pc = address;
        }
    }

    /// stores a value into the stack
    fn push_value(&mut self, val: Value) {
        match self.value_stacks.last_mut() {
            Some(ref mut stack) => stack.push(val),
            None => self.value_stacks.push(vec![val]),
        }
    }

    /// returns the last pushed value
    fn pop_value(&mut self) -> Option<Value> {
        self.value_stacks.last_mut()?.pop()
    }

    /// returns ref to the last pushed value
    fn last_value(&mut self) -> Option<&Value> {
        self.value_stacks.last_mut()?.last()
    }

    pub fn exec_step(&mut self) -> Result<ExecStatus, ExecutionError> {
        use Instruction::*;
        // fetch current expression
        let pc = *self.execution_pointers.last()
            .ok_or(exec_error!("No instruction pointer left."))?;
        if pc >= self.instructions.len() {
            return Ok(ExecStatus::Finished);
        }
        let current_instruction = self.instructions.get(pc)
            .ok_or(exec_error!("reached end of instructions"))?;
        // evaluate it
        match *current_instruction { 
            // Jumps
            Goto(addr) => {
                self.goto(addr);
                return Ok(ExecStatus::Running);
            },
            GotoIf(addr) => {
                // cast top of the stack as bool
                let val_as_bool: bool = self.last_value().ok_or(exec_error!("Empty stack"))?.into();
                if val_as_bool {
                    self.goto(addr);
                    return Ok(ExecStatus::Running);
                } 
            },
            // Refs to Values
            NewRef(ref name) => self.create_ref(name)?,
            Load(ref name) => {
                let value = self.load(name);
                self.push_value(value);
            },
            Store(ref name) => {
                let value = self.pop_value().ok_or(exec_error!("Empty stack, nothing to store"))?;
                self.store(name, value)?;
            },
            Val(ref value) => self.push_value(value.clone()),
            // Functions
            FnCall => {
                let value = self.pop_value().ok_or(exec_error!("Empty stack, nothing to call"))?; 
                if let Value::Function(addr) = value {
                    // go to the function body
                    self.execution_pointers.push(addr);
                    return Ok(ExecStatus::Running);
                } else {
                    return Err(exec_error!("value is not callable"));
                }
            },
            FnRet => {
                let _ = self.execution_pointers.pop();
            },
            // Stack management
            PushToNext(nb) => {
                for _ in 0..nb {
                    let value = self.pop_value().ok_or(exec_error!("no value to passthrough"))?;
                    self.passthrough.push(value);
                }
            },
            NewStack => {
                self.bindings.push(HashMap::new()); // new bindings
                // pass the values to the new stack
                let mut values = Vec::new();
                while let Some(value) = self.passthrough.pop() {
                    values.push(value);
                }
                self.value_stacks.push(values); // new stack
            },
            ClearStack => self.value_stacks.last_mut().ok_or(exec_error!("no stack to clear"))?.clear(),
            DelStack => {
                self.bindings.pop().ok_or(exec_error!("Empty binding stack"))?;
                self.value_stacks.pop().ok_or(exec_error!("Empty stack, cannot delete it"))?;
                // pass the values to the current stack
                while let Some(value) = self.passthrough.pop() {
                    self.value_stacks.last_mut()
                        .ok_or(exec_error!("no value stack left"))?
                        .push(value);
                }
            },
            And => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(Value::Bool((&left_hand).into() && (&right_hand).into()));
            },
            Or => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(Value::Bool((&left_hand).into() || (&right_hand).into()));
            },
            Add => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(left_hand + right_hand);
            },

            _ => {},
        }
        self.goto_next();
        Ok(ExecStatus::Running)
    }

    pub fn execute(&mut self) -> Result<Option<Value>, ExecutionError> {
        loop {
            match self.exec_step() {
                Err(e) => return Err(e),
                Ok(ExecStatus::Running) => continue,
                Ok(ExecStatus::Finished) => break,
            }
        }
        Ok(self.pop_value())
    }
}
