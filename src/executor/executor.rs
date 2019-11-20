use std::collections::HashMap;
use crate::compiler::Instruction;
use crate::builtins::Value;
use crate::executor::contexts::{
    Context,
    Scope,
    ScopeKind,
    ContextError,
};

#[derive(Debug)]
pub struct ExecutionError {
    pub msg: String    
}
impl std::error::Error for ExecutionError {}
impl std::fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg,)
    }
}
impl From<ContextError> for ExecutionError {
    fn from(err: ContextError) -> Self {
        Self { msg: err.msg }
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
    context: Context,
    passthrough: Vec<Value>,
    execution_pointers: Vec<usize>,
}

impl <'inst> Executor <'inst> { 
    pub fn from_instructions(instructions: &'inst [Instruction]) -> Self {
        Self { 
            instructions: instructions,
            passthrough: Vec::new(),
            execution_pointers: vec![0],
            context: Context::new(),
        }
    }

    /// load the value referenced by `name`
    fn load(&self, name: &str) -> Value {
        match self.context.load(name) {
            Ok(v) => v,
            Err(_) => Value::Undefined,

        }
    }
    
    /// store a value at the referenced place
    /// If the reference doesn't points to anything, returns an error
    fn store(&mut self, name: &str, val: Value) -> Result<(), ExecutionError> {
        self.context.store(name, val).map_err(Into::into)
    }
    
    /// create a reference
    fn create_ref(&mut self, name: &str) -> Result<(), ExecutionError> {
        self.context.create_ref(name).map_err(Into::into)
    }

    /// stores a value into the stack
    fn push_value(&mut self, val: Value) {
        self.context.push_value(val);
    }

    /// returns the last pushed value
    fn pop_value(&mut self) -> Result<Value, ExecutionError> {
        match self.context.pop_value() {
            Some(value) => Ok(value),
            None => Err(exec_error!("empty stack"))
        }
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

    /// Return current the execution pointer
    fn instruction_pointer(&mut self) -> Result<usize, ExecutionError> {
        let ip: &usize = self.execution_pointers
            .last()
            .ok_or(exec_error!("No instruction pointer left."))?;
        Ok(*ip)
    }

    /// Load an instruction, and execute it
    pub fn exec_step(&mut self) -> Result<ExecStatus, ExecutionError> {
        use Instruction::*;
        use Value::*;
        // fetch current expression
        let ip = self.instruction_pointer()?;
        if ip >= self.instructions.len() {
            return Ok(ExecStatus::Finished);
        }
        let current_instruction = self.instructions.get(ip)
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
                let value_bool: bool = (&self.pop_value()?).into();
                // re-insert the boolean back onto the stack
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
                let value = self.pop_value()?;
                self.store(name, value)?;
            },
            Val(ref value) => self.push_value(value.clone()),
            // Functions
            FnCall => {
                let value = self.pop_value()?; 
                if let Value::Function(addr) = value {
                    // go to the function body
                    self.execution_pointers.push(addr);
                    // Add a new scope
                    self.context.add_function_scope();
                    // pass in arguments
                    while let Some(value) = self.passthrough.pop() {
                        self.push_value(value);
                    }
                    return Ok(ExecStatus::Running);
                } else {
                    return Err(exec_error!("value is not callable"));
                }
            },
            FnRet => {
                self.context.pop_function_scope();
                // pass result values
                while let Some(value) = self.passthrough.pop() {
                    self.push_value(value);
                }
                let _ = self.execution_pointers.pop();
            },
            // LoopMangement
            NewLoopStack => self.context.add_loop_scope(),
            DelLoopStack => self.context.pop_loop_scope(),
            // Stack management
            PushToNext(nb) => {
                for _ in 0..nb {
                    let value = self.pop_value()?;
                    self.passthrough.push(value);
                }
            },
            NewStack => {
                self.context.add_block_scope();
                while let Some(value) = self.passthrough.pop() {
                    self.push_value(value);
                }
            },
            ClearStack => self.context.flush_values(),
            DelStack => {
                let _ = self.context.pop_scope()
                    .ok_or(exec_error!("No scope in context"))?;
                // pass the values to the current stack
                while let Some(value) = self.passthrough.pop() {
                    self.push_value(value);
                }
            },
            And => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(Value::Bool((&left_hand).into() && (&right_hand).into()));
            },
            Or => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(Value::Bool((&left_hand).into() || (&right_hand).into()));
            },
            // arithemic operations
            Add => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand + right_hand);
            },
            Sub => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand - right_hand);
            },
            Mul => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand * right_hand);
            },
            Div => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand / right_hand);
            },
            Mod => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand % right_hand);
            },
            Pow => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand.pow(&right_hand));
            },
            // Comparison
            Equal => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand.is_equal(&right_hand));
            },
            NotEqual => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(!left_hand.is_equal(&right_hand));
            },
            GreaterThan => {
                let left_float: f64 = (&self.pop_value()?).into();
                let right_float: f64 = (&self.pop_value()?).into();
                self.push_value(Bool(left_float > right_float));
            },
            GreaterThanOrEqual => {
                let left_float: f64 = (&self.pop_value()?).into();
                let right_float: f64 = (&self.pop_value()?).into();
                self.push_value(Bool(left_float >= right_float));
            },
            LessThan => {
                let left_float: f64 = (&self.pop_value()?).into();
                let right_float: f64 = (&self.pop_value()?).into();
                self.push_value(Bool(left_float < right_float));
            },
            LessThanOrEqual => {
                let left_float: f64 = (&self.pop_value()?).into();
                let right_float: f64 = (&self.pop_value()?).into();
                self.push_value(Bool(left_float <= right_float));
            },
            // Unary operations
            Not => {
                let value = self.pop_value()?;
                self.push_value(!value);
            },
            Plus => {
                let value: f64 = (&self.pop_value()?).into();
                self.push_value(Num(value));
            },
            Minus => {
                let value: f64 = (&self.pop_value()?).into();
                self.push_value(Num(-value));
            },
        }
        self.goto_next();
        Ok(ExecStatus::Running)
    }

    /// Execute all instructions
    /// return the top of the value stack
    pub fn execute(&mut self) -> Result<Option<Value>, ExecutionError> {
        loop {
            match self.exec_step() {
                Err(e) => return Err(e),
                Ok(ExecStatus::Running) => continue,
                Ok(ExecStatus::Finished) => break,
            }
        }
        match self.pop_value() {
            Ok(value) => Ok(Some(value)),
            Err(_) => Ok(None),
        }
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
        let ast = Ast::from_str(&src).expect("Could not build ast");
        // Compile into instructions
        let instructions = Compiler::compile(&ast.root).expect("compiler error");
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
    #[test]
    fn test_while() {
        let val = exec(r#"
        let run_flag = 100;
        let counter = 0;
        while (run_flag) {
            run_flag = run_flag -1;
            counter = counter + 1;
        }
        counter
        "#).unwrap();
        assert_eq!(val, Some(Num(100.)));
    }
    #[test]
    fn test_function() {
        // classic call
        let val = exec(r#"
        function f() {
            return true;
        }
        f()
        "#).unwrap();
        assert_eq!(val, Some(Bool(true)));
        // early return
        let val = exec(r#"
        function f(arg) {
            if (arg <= 1) {
                return true;
            }
            return false;
        }
        f(0)
        "#).unwrap();
        assert_eq!(val, Some(Bool(true)));
        // recursion
        let val = exec(r#"
        let counter = 0;
        function f(arg) {
            counter = counter + 1;
            if (arg == 0) {
                return true;
            }
            return f(arg - 1);
        }
        f(9);
        counter
        "#).unwrap();
        assert_eq!(val, Some(Num(10.)));
    }
    #[test]
    fn test_lower_than() {
        let val = exec("1 < 2 ").unwrap();
        assert_eq!(val, Some(Bool(true)));
    }
    #[test]
    fn test_break() {
        let val = exec(r#"
        let counter = 0;
        while (true) {
            if (counter > 2) {
                break;
            }
            counter = counter + 1;
        }
        counter
        "#).unwrap();
        assert_eq!(val, Some(Num(3.)));
    }
    #[test]
    fn test_continue() {
        let val = exec(r#"
        let counter = 0;
        while (counter < 2) {
            counter = counter + 1;
            continue;
            counter = 300;
        }
        counter
        "#).unwrap();
        assert_eq!(val, Some(Num(2.)));
    }
}

