

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum KeywordKind {
    /// The `else` keyword
    Else,
    /// The `fn` keyword
    Function,
    /// The `if` keyword
    If,
    /// The `let` keyword
    Let,
    /// The `return` keyword
    Return,
    /// The `while` keyword
    While,
}

/// Punctuation
#[derive(Clone, PartialEq, Debug)]
pub enum SeparatorKind {
    /// `}`
    CloseBlock,
    /// `)`
    CloseParen,
    /// `{`
    OpenBlock,
    /// `(`
    OpenParen,
    /// `;`
    Semicolon,
    /// `,`
    Comma,
}

#[derive(PartialEq, Clone, Copy, Debug)]
/// Operator
pub enum OperatorKind {
    /// `+`
    Add,
    /// `&`
    And,
    /// `=`
    Assign,
    /// `/`
    Div,
    /// `==`
    Equal,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanOrEq,
    /// `<`
    LessThan,
    /// `<=`
    LessThanOrEq,
    /// `%`
    Mod,
    /// `*`
    Mul,
    /// `!`
    Not,
    /// `!=`
    NotEq,
    /// `|`
    Or,
    /// `**`
    Pow,
    /// `-`
    Sub,
}
/// Literals
#[derive(PartialEq, Clone, Debug)]
pub enum LiteralKind {
    /// int or float
    Numeric(f64),
    /// boolean `true` or `false`
    Boolean(bool),
}

#[derive(Clone, PartialEq, Debug)]
/// each variant represents a piece of `toylang` syntax
pub enum TokenKind {
    /// An identifier
    Literal(LiteralKind),
    /// A keyword
    Keyword(KeywordKind),
    /// A separator
    Separator(SeparatorKind),
    /// An operator
    Operator(OperatorKind),
    /// any identifier, variable name, function name ...
    Identifier(String),
    /// A comment
    Comment(String),
}

#[derive(Clone, PartialEq, Debug)]
/// A struct that describes a position in source code.
pub struct SrcCursor {
    pub line: usize,
    pub column: usize,
    pub idx: usize,
}
impl SrcCursor {
    pub fn new() -> Self {
        Self {
            column: 0,
            idx: 0,
            line: 0,
        }
    }
    pub fn consume_n(&mut self, n: usize) {
        self.idx += n;
        self.column += n;
    }
    pub fn new_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

#[derive(Clone, PartialEq, Debug)]
/// A piece of `toylang` syntax
pub struct Token {
    /// the token kind
    pub kind: TokenKind,
    /// describes where the token was in the source code
    pub cursor: SrcCursor
}
impl Token {
    pub fn new(kind: TokenKind, cursor: &SrcCursor) -> Self {
        Self {
            kind: kind,
            cursor: cursor.clone()
        }
    }
}
