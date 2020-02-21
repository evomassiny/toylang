use crate::builtins::{Value,FnKind,LexicalLabel};


/// This is the complete set of instructions generated by the compiler.
/// This is what the interpretor actually executes.
#[derive(Debug,Clone,PartialEq)]
pub enum Instruction {
    /// jump to an address
    Goto(usize),
    /// pop a value, cast it to `bool`, if it's true, jump to the address,
    /// in any cases, push the value back to stack
    GotoIf(usize),
    /// Create a new variable (in the current context)
    NewRef(String), 
    /// Loads a Value -indentified by its name- by preforming lookups on the scope chain, starting
    /// from the current
    Load(String),
    /// pop the stack and store the value into the variable pointed by `String`
    /// if the stack is empty, store `Value::Undefined`
    Store(String), 
    /// Creates a new context, and give it a label
    NewContext(LexicalLabel), 
    /// Creates a new function, and bind it to the current context
    NewFunction(FnKind), 
    /// Creates a value, and push it into the stack
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
    NewLoopCtx,
    /// remove the last loop stack
    DelLoopCtx,
    /// PopToLoopStack
    PopToLoopCtx,
    /// pop 2 values from the stack, and push their sum 
    Add,
    /// pop 2 values from the stack, and push their substraction 
    Sub,
    /// pop 2 values from the stack, and push their ratio 
    Div,
    /// pop 2 values from the stack, and push their product 
    Mul,
    /// pop 2 values from the stack, and push one power to the other
    Pow,
    /// pop 2 values from the stack, and push the rest of the division of one by the other
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
