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
    /// Creates a new Function scope
    /// then, if any element are in the passthrough buffer push those elements to it
    FnCall, // pop stack, push it to PC stack
    /// Exit a function
    /// Pop each scopes until a function scope has been poped
    /// then, if any element are in the passthrough buffer push those elements to the new current
    /// scope
    FnRet,  
    /// move the `n` last elements of the current scope to a passthrough buffer
    PushToNext(usize),
    /// Creates a new block scope
    /// then, if any element are in the passthrough buffer push those elements to it
    NewStack,
    /// clear all values of the current stack
    ClearStack,
    /// remove the last stack
    /// then if any element are in the passthrough buffer push those elements to the current one
    DelStack,
    /// Creates a new Loop scope
    NewLoopStack,
    /// remove the last loop stack
    DelLoopStack,
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
