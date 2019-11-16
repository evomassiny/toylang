use std::collections::HashMap;
use crate::compiler::Instruction;
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
            GotoIf(addr) => { // Does not consume the value but cast it as a bool
                // pop stack, and cast as bool
                let value_bool: bool = (
                        &self.pop_value()
                        .ok_or(exec_error!("Empty stack"))?
                    ).into();
                // re-insert the boolean back onto the stackœ:w
                self.push_value(Value::Bool(value_bool));
                if value_bool {
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
            // arithemic operations
            Add => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(left_hand + right_hand);
            },
            Sub => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(left_hand - right_hand);
            },
            Mul => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(left_hand * right_hand);
            },
            Div => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(left_hand / right_hand);
            },
            Mod => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(left_hand % right_hand);
            },
            Pow => {
                let left_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                let right_hand = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(left_hand.pow(&right_hand));
            },

            Not => {
                let value = self.pop_value().ok_or(exec_error!("Value stack empty"))?;
                self.push_value(!value);
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

#[cfg(test)]
mod tests {
    use crate::ast::Ast;
    use crate::compiler::Compiler;
    use crate::executor::{Executor,ExecutionError};
    use crate::builtins::Value;
    use Value::*;

    fn exec(src: &str) -> Result<Option<Value>, ExecutionError> {
        // parse the ast
        let ast = Ast::from_str(&src).unwrap();
        // Compile into instructions
        let instructions = Compiler::compile(&ast.root).unwrap();
        // execute the instructions
        let mut executor = Executor::from_instructions(&instructions);
        executor.execute()
    }

    #[test]
    fn test_boolean_cast() {
        assert_eq!(exec("0 && true").unwrap(), Some(Bool(false)));
        assert_eq!(exec("0.1 && true").unwrap(), Some(Bool(true)));
        assert_eq!(exec(r#" "" && true"#).unwrap(), Some(Bool(false)));
        assert_eq!(exec(r#" "content" && true"#).unwrap(), Some(Bool(true)));
        assert_eq!(exec(" null && true").unwrap(), Some(Bool(false)));
        assert_eq!(exec(" undefined && true").unwrap(), Some(Bool(false)));
        assert_eq!(exec("function foo(){}; foo && true").unwrap(), Some(Bool(true)));
    }
    #[test]
    fn test_and() {
        assert_eq!(exec("true && true").unwrap(), Some(Bool(true)));
        assert_eq!(exec("false && true").unwrap(), Some(Bool(false)));
        assert_eq!(exec("false && false").unwrap(), Some(Bool(false)));
        assert_eq!(exec("true && false").unwrap(), Some(Bool(false)));
        // test shortcut evaluation
        // if foo() is evaluated, it fails because it's not defined
        assert_eq!(exec(" false && foo()").unwrap(), Some(Bool(false)));
        assert!(exec(" true && foo()").is_err()); 
    }
    #[test]
    fn test_or() {
        assert_eq!(exec("true || true").unwrap(), Some(Bool(true)));
        assert_eq!(exec("false || true").unwrap(), Some(Bool(true)));
        assert_eq!(exec("false || false").unwrap(), Some(Bool(false)));
        assert_eq!(exec("true || false").unwrap(), Some(Bool(true)));
        // test shortcut evaluation
        // if foo() is evaluated, it fails because it's not defined
        assert_eq!(exec(" true || foo()").unwrap(), Some(Bool(true)));
        assert!(exec("false || foo()").is_err()); 
    }

    #[test]
    fn test_add() {
        assert_eq!(
            exec("1 + 2").unwrap(),
            Some(Num(3.))
        );
        let val: f64 = (&exec("undefined + 2").unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec("null + 2").unwrap(),
            Some(Num(2.))
        );
        assert_eq!(
            exec(r#" "str" + 2"#).unwrap(),
            Some(Str("str2".into()))
        );
        assert_eq!(
            exec(r#" true + 2"#).unwrap(),
            Some(Num(3.))
        );
        assert_eq!(
            exec(r#" false + 2"#).unwrap(),
            Some(Num(2.))
        );
    }
    #[test]
    fn test_sub() {
        assert_eq!(
            exec("1 - 2").unwrap(),
            Some(Num(-1.))
        );
        let val: f64 = (&exec("undefined - 2").unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec("null - 2").unwrap(),
            Some(Num(-2.))
        );
        let val: f64 = (&exec(r#" "str" - 2"#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        let val: f64 = (&exec(r#"
        function foo() { return 1; }
        foo -1
        "#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec(r#" true - 2"#).unwrap(),
            Some(Num(-1.))
        );
        assert_eq!(
            exec(r#" false - 2"#).unwrap(),
            Some(Num(-2.))
        );
    }
    #[test]
    fn test_mul() {
        assert_eq!(
            exec("1 * 2").unwrap(),
            Some(Num(2.))
        );
        let val: f64 = (&exec("undefined * 2").unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec("null * 2").unwrap(),
            Some(Num(0.))
        );
        let val: f64 = (&exec(r#" "str" * 2"#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        let val: f64 = (&exec(r#"
        function foo() { return 1; }
        foo *1
        "#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec(r#" true * 2"#).unwrap(),
            Some(Num(2.))
        );
        assert_eq!(
            exec(r#" false * 2"#).unwrap(),
            Some(Num(0.))
        );
    }
    #[test]
    fn test_div() {
        assert_eq!(
            exec("6 / 2").unwrap(),
            Some(Num(3.))
        );
        let val: f64 = (&exec("undefined / 2").unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec("null / 2").unwrap(),
            Some(Num(0.))
        );
        let val: f64 = (&exec(r#" "str" / 2"#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        let val: f64 = (&exec(r#"
        function foo() { return 1; }
        foo / 1
        "#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec(r#" true / 2"#).unwrap(),
            Some(Num(0.5))
        );
        assert_eq!(
            exec(r#" false / 2"#).unwrap(),
            Some(Num(0.))
        );
    }
    #[test]
    fn test_mod() {
        assert_eq!(
            exec("7 % 2").unwrap(),
            Some(Num(1.))
        );
        let val: f64 = (&exec("undefined % 2").unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec("null % 2").unwrap(),
            Some(Num(0.))
        );
        let val: f64 = (&exec(r#" "str" % 2"#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        let val: f64 = (&exec(r#"
        function foo() { return 1; }
        foo % 1
        "#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec(r#" true % 2"#).unwrap(),
            Some(Num(1.))
        );
        assert_eq!(
            exec(r#" false % 2"#).unwrap(),
            Some(Num(0.))
        );
    }
    #[test]
    fn test_pow() {
        assert_eq!(
            exec("7 ** 2").unwrap(),
            Some(Num(49.))
        );
        let val: f64 = (&exec("undefined ** 2").unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec("null ** 2").unwrap(),
            Some(Num(0.))
        );
        let val: f64 = (&exec(r#" "str" ** 2"#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        let val: f64 = (&exec(r#"
        function foo() { return 1; }
        foo % 1
        "#).unwrap().unwrap()).into();
        assert!(val.is_nan());
        assert_eq!(
            exec(r#" true ** 2"#).unwrap(),
            Some(Num(1.))
        );
        assert_eq!(
            exec(r#" false ** 2"#).unwrap(),
            Some(Num(0.))
        );
    }

    #[test]
    fn test_if() {
        let val = exec(r#"
        function test(v) { 
            if (v) {
                return true;
            } else {
                return false;
            }
        }
        test(true)
        "#).unwrap();
        assert_eq!(val, Some(Bool(true)));
        let val = exec(r#"
        function test(v) { 
            if (v) {
                return true;
            } else {
                return false;
            }
        }
        test(false)
        "#).unwrap();
        assert_eq!(val, Some(Bool(false)));
    }
}

