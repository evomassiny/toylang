use crate::builtins::Value;

#[derive(Debug,Clone,PartialEq)]
pub enum Instruction {
    /// jump to an address
    Goto(usize),
    /// pop a value, cast it to `bool`, if it's true, jump to the address,
    /// in any cases, push the value back to stack
    GotoIf(usize),
    /// Create a new variable binding
    NewRef(String), 
    Load(String),
    /// pop the stack and store the value into the variable pointed by `String`
    /// if the stack is empty, store `Value::Undefined`
    Store(String), 
    // value 
    Val(Value),
    // Arguments and return value
    /// Call a function
    FnCall, // pop stack, push it to PC stack
    /// Exit a function
    FnRet,  // pop PC stack
    // stack machine states
    /// move the `n` last elements of the current stack to a passthrough buffer
    PushToNext(usize),
    /// Creates a new stack
    /// then, if any element are in the passthrough buffer push those elements to it
    NewStack,
    /// clear all values of the current stack
    ClearStack,
    /// remove the last stack
    /// then if any element are in the passthrough buffer push those elements to the current one
    DelStack,
    // Binary Instructions
    // * Numerical
    Add,
    Sub,
    Div,
    Mul,
    Pow,
    Mod,
    // * Comparison
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    // * Logical
    And,
    Or,
    // Unary Instructions
    Minus,
    Plus,
    Not,
}

impl Into<bool> for &Value {
    fn into(self) -> bool {
        use Value::*;
        match *self {
            Str(ref s) => !s.is_empty(),
            Num(n) => n > 0., // 
            Bool(b) => b,
            Function(_) => true,
            NaN => false,
            Null | Undefined => false,
        }
    }
}

