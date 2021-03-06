use crate::compiler::Instruction;
use crate::builtins::{
    FnKind,
    Value,
};
use crate::executor::contexts::{
    Context,
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

/// Describes the states of an Exectutor
pub enum ExecStatus {
    Running, 
    Finished,
}

/// A byte code interpreter
pub struct Executor<'inst> {
    /// A slice of byte code Instruction
    pub instructions: &'inst [Instruction],
    /// The execution context: the script memory
    pub context: Context,
    /// an internal buffer, uses to store values between contexts
    passthrough: Vec<Value>,
    /// the script execution stack
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
    fn push_value(&mut self, val: Value) -> Result<(), ExecutionError> {
        self.context.push_value(val).map_err(Into::into)
    }

    /// returns the last pushed value
    fn pop_value(&mut self) -> Result<Value, ExecutionError> {
        match self.context.pop_value() {
            Some(value) => Ok(value),
            None => Err(exec_error!("Cannot pop value from empty stack"))
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

    /// Return the current execution pointer
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
                self.push_value(Value::Bool(value_bool))?;
                if value_bool {
                    self.goto(addr);
                    return Ok(ExecStatus::Running);
                } 
            },
            NewFunction(ref fn_kind) => {
                // load the current idx
                let ctx_id = self.context.current_context()?;
                // create a function from it and push it to the stack
                self.push_value(Value::Function(fn_kind.clone(), ctx_id))?;
            },
            NewRef(ref name) => self.create_ref(name)?,
            Load(ref name) => {
                let value = self.load(name);
                self.push_value(value)?;
            },
            Store(ref name) => {
                let value = self.pop_value()?;
                self.store(name, value)?;
            },
            Val(ref value) => self.push_value(value.clone())?,
            // Functions Context management
            FnCall => {
                let value = self.pop_value()?; 
                match value {
                    Value::Function(FnKind::Address(addr), ctx_id) => {
                        // go to the function body
                        self.execution_pointers.push(addr);
                        // change execution context
                        self.context.add_binded_context(ctx_id);
                        // pass in arguments
                        while let Some(value) = self.passthrough.pop() {
                            self.push_value(value)?;
                        }
                        return Ok(ExecStatus::Running);
                    },
                    Value::Function(FnKind::Native(func), ctx_id) => {
                        // change execution context
                        self.context.add_binded_context(ctx_id);
                        // pass in arguments
                        let mut args = Vec::new();
                        while let Some(value) = self.passthrough.pop() {
                            args.push(value);
                        }
                        self.push_value(func(&mut args))?;
                    },
                    _ => return Err(exec_error!("value is not callable")),
                }
            },
            FnRet => {
                // got back to previous execution point
                let _ = self.execution_pointers.pop();
                // got back to previous execution context
                self.context.pop_context();
                // pass result values
                while let Some(value) = self.passthrough.pop() {
                    self.push_value(value)?;
                }
            },
            // Loop Context Management
            NewLoopCtx => self.context.add_loop_stack()?,
            DelLoopCtx => self.context.pop_loop_stack()?,
            PopToLoopCtx => self.context.pop_until_loop_stack()?,
            // Stack management
            PushToNext(nb) => {
                for _ in 0..nb {
                    let value = self.pop_value()?;
                    self.passthrough.push(value);
                }
            },
            NewStack => {
                self.context.add_block_stack()?;
                while let Some(value) = self.passthrough.pop() {
                    self.push_value(value)?;
                }
            },
            ClearStack => self.context.flush_values()?,
            DelStack => {
                let _ = self.context.pop_stack()
                    .ok_or(exec_error!("No stack in context"))?;
                // pass the values to the current stack
                while let Some(value) = self.passthrough.pop() {
                    self.push_value(value)?;
                }
            },
            And => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(Value::Bool((&left_hand).into() && (&right_hand).into()))?;
            },
            Or => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(Value::Bool((&left_hand).into() || (&right_hand).into()))?;
            },
            // arithemic operations
            Add => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand + right_hand)?;
            },
            Sub => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand - right_hand)?;
            },
            Mul => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand * right_hand)?;
            },
            Div => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand / right_hand)?;
            },
            Mod => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand % right_hand)?;
            },
            Pow => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand.pow(&right_hand))?;
            },
            // Comparison
            Equal => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(left_hand.is_equal(&right_hand))?;
            },
            NotEqual => {
                let left_hand = self.pop_value()?;
                let right_hand = self.pop_value()?;
                self.push_value(!left_hand.is_equal(&right_hand))?;
            },
            GreaterThan => {
                let left_float: f64 = (&self.pop_value()?).into();
                let right_float: f64 = (&self.pop_value()?).into();
                self.push_value(Bool(left_float > right_float))?;
            },
            GreaterThanOrEqual => {
                let left_float: f64 = (&self.pop_value()?).into();
                let right_float: f64 = (&self.pop_value()?).into();
                self.push_value(Bool(left_float >= right_float))?;
            },
            LessThan => {
                let left_float: f64 = (&self.pop_value()?).into();
                let right_float: f64 = (&self.pop_value()?).into();
                self.push_value(Bool(left_float < right_float))?;
            },
            LessThanOrEqual => {
                let left_float: f64 = (&self.pop_value()?).into();
                let right_float: f64 = (&self.pop_value()?).into();
                self.push_value(Bool(left_float <= right_float))?;
            },
            // Unary operations
            Not => {
                let value = self.pop_value()?;
                self.push_value(!value)?;
            },
            Plus => {
                let value: f64 = (&self.pop_value()?).into();
                self.push_value(Num(value))?;
            },
            Minus => {
                let value: f64 = (&self.pop_value()?).into();
                self.push_value(Num(-value))?;
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

    /// Call a function defined in self.instructions
    pub fn call_function(&mut self, name: &str, mut args: Vec<Value>) -> Result<Option<Value>, ExecutionError> {
        // assert that the script has ran
        if let Some(ip) = self.execution_pointers.last() {
            if *ip <= self.execution_pointers.len() {
                self.execute()?;
            }
        } else {
            self.execute()?;
        }
        // perform the call
        match self.load(name) {
            Value::Function(FnKind::Address(addr), ctx_id) => {
                // go to the function body
                self.execution_pointers.push(addr);
                // change execution context
                self.context.add_binded_context(ctx_id);
                // pass in arguments
                while let Some(arg) = args.pop() {
                    let _ = self.push_value(arg);
                }
                return self.execute();
            },
            Value::Function(FnKind::Native(func), _) => {
                let v = func(&mut args);
                return Ok(Some(v));
            },
            v => return Err(exec_error!(format!("{} not callable, it's: {}", name, v))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use crate::ast::Ast;
    use crate::executor::{Executor,ExecutionError};
    use crate::compiler::Compiler;
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
    fn test_pre_increment() {
        assert_eq!(
            exec("let a = 1; ++a").unwrap(),
            Some(Num(2.))
        );
        assert_eq!(
            exec("let a = 1; ++a; a").unwrap(),
            Some(Num(2.))
        );
    }
    #[test]
    fn test_pre_decrement() {
        assert_eq!(
            exec("let a = 1; --a").unwrap(),
            Some(Num(0.))
        );
        assert_eq!(
            exec("let a = 1; --a; a").unwrap(),
            Some(Num(0.))
        );
    }
    #[test]
    fn test_post_increment() {
        assert_eq!(
            exec("let a = 1; a++").unwrap(),
            Some(Num(1.))
        );
        assert_eq!(
            exec("let a = 1; a++; a").unwrap(),
            Some(Num(2.))
        );
    }
    #[test]
    fn test_post_decrement() {
        assert_eq!(
            exec("let a = 1; a--").unwrap(),
            Some(Num(1.))
        );
        assert_eq!(
            exec("let a = 1; a--; a").unwrap(),
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
        // test nested loops
        let val = exec(r#"
        let counter = 0;
        while (true) {
            if (counter > 2) {
                while (true) {
                    if (counter < -20) {
                        break;
                    }
                    counter = counter - 1;
                }
                break;
            }
            counter = counter + 1;
        }
        counter
        "#).unwrap();
        assert_eq!(val, Some(Num(-21.)));
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
    #[test]
    fn test_native_fn() {
        // native function without argument
        fn give_true(_ : &mut Vec<Value>) -> Value { Bool(true) }
        
        let src = "let a = give_true(); a";
        let ast = Ast::from_str(&src).expect("Could not build ast");
        let instructions = Compiler::compile(&ast.root).expect("compiler error");
        let mut executor = Executor::from_instructions(&instructions);
        // add the function
        let _ = executor.context.add_native_function("give_true", give_true);

        assert_eq!(
            executor.execute().unwrap(),
            Some(Bool(true))
        );
        
        // native function with argument
        fn add_two(val : &mut Vec<Value>) -> Value { 
            if let Some(Num(num)) = val.pop() {
                return Num(num + 2.);
            }
            Null
        }
        let src = "let a = add_two(22); a";
        let ast = Ast::from_str(&src).expect("Could not build ast");
        let instructions = Compiler::compile(&ast.root).expect("compiler error");
        let mut executor = Executor::from_instructions(&instructions);
        // add the function
        let _ = executor.context.add_native_function("add_two", add_two);

        assert_eq!(
            executor.execute().unwrap(),
            Some(Num(24.))
        );
    }

    #[test]
    fn test_script_function_call() {
        let src = r#"
        function add_two(a) {
            return a + 2;
        }
        "#;
        let ast = Ast::from_str(&src).expect("Could not build ast");
        let instructions = Compiler::compile(&ast.root).expect("compiler error");
        let mut executor = Executor::from_instructions(&instructions);
        let _ = executor.execute();

        assert_eq!(
            executor.call_function(
                "add_two",
                vec![Num(22.)],
            ).unwrap(),
            Some(Num(24.))
        );

        // check if argument order is preserved
        let src = r#"
        function add_two_to_2nd(a, b) {
            return b + 2;
        }
        "#;
        let ast = Ast::from_str(&src).expect("Could not build ast");
        let instructions = Compiler::compile(&ast.root).expect("compiler error");
        let mut executor = Executor::from_instructions(&instructions);
        let _ = executor.execute();

        assert_eq!(
            executor.call_function(
                "add_two_to_2nd",
                vec![Bool(true), Num(22.)],
            ).unwrap(),
            Some(Num(24.))
        );
    }

    #[test]
    fn test_closure() {
        let src = r#"
        function level_0() {
             let a = 1;
             function level_1() {
                 a = a + 1; 
                 return a;
             }
             return level_1;
        }
        let f = level_0();
        f();
        f()
        "#;
        let ast = Ast::from_str(&src).expect("Could not build ast");
        let instructions = Compiler::compile(&ast.root).expect("compiler error");
        let mut executor = Executor::from_instructions(&instructions);

        assert_eq!(
            executor.execute().unwrap(),
            Some(Num(3.))
        );
        // same as above but assert that each closure has a different scope chain
        let src = r#"
        function level_0() {
             let a = 1;
             function level_1() {
                 a = a + 1; 
                 return a;
             }
             return level_1;
        }
        let foo = level_0();
        foo();
        let bar = level_0();
        bar();
        bar()
        "#;
        let ast = Ast::from_str(&src).expect("Could not build ast");
        let instructions = Compiler::compile(&ast.root).expect("compiler error");
        let mut executor = Executor::from_instructions(&instructions);

        assert_eq!(
            executor.execute().unwrap(),
            Some(Num(3.)),
        );
    }

    #[test]
    fn test_context_switch() {
        // test context switching with several call to the same function
        // within the same context
        let src = r#"
        function fib(n) {
            if (n < 2) {
                return n;
            }
            return fib(n - 1) + fib(n - 2);
        }

        let i = 0;
        let result = undefined;
        while (true) {
            result = fib(i);

            if (i++ == 5) {
                break;
            }
        }
        result
        "#;
        let ast = Ast::from_str(&src).expect("Could not build ast");
        let instructions = Compiler::compile(&ast.root).expect("compiler error");
        let mut executor = Executor::from_instructions(&instructions);

        assert_eq!(
            executor.execute().unwrap(),
            Some(Num(5.)),
        );

    }
}

