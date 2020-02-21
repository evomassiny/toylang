/// this modules should gather all Javascript builtins types and methods
mod values;
pub use values::{
    Value,
    FnKind,
    NativeFn,
    ContextID,
    LexicalLabel,
};
