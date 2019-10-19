use std::error::Error;
use std::fmt;
use regex::Regex;
use crate::tokens::{
    TokenKind,
    Token,
    LiteralKind,
    SeparatorKind,
    OperatorKind,
    KeywordKind,
    SrcCursor
};

/// An error that occurred during lexing or compiling of the source input.
#[derive(Clone)]
pub struct LexingError {
    msg: String,
    cursor: SrcCursor
}

impl LexingError {
    fn new(msg: String, cursor: &SrcCursor) -> Self {
        Self {
            msg: msg,
            cursor: cursor.clone(),
        }
    }
}
impl fmt::Display for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at line {}, column {}",
               self.msg,
               self.cursor.line + 1,
               self.cursor.column + 1,
           )
    }
}
impl fmt::Debug for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at line {}, column {}",
               self.msg,
               self.cursor.line + 1,
               self.cursor.column + 1,
           )
    }
}
impl Error for LexingError {}



/// Tokenize `src` into a Vec of Token
pub fn lex(src: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    use TokenKind::*;
    use KeywordKind::*;
    use LiteralKind::*;
    use SeparatorKind::*;
    use OperatorKind::*;
    // Each regex defines a Variant (or subvariant) of a TokenKind syntax
    lazy_static! {
        static ref NEW_LINE_PATTERN: Regex = Regex::new(r"^(?P<token>(\r?\n)+)").unwrap();
        static ref WHITE_SPACE_PATTERN: Regex = Regex::new(r"^(?P<token>\s+)").unwrap();
        static ref BOOLEAN_PATTERN: Regex = Regex::new(r"^(?P<token>true|false)").unwrap();
        static ref KEYWORD_PATTERN: Regex = Regex::new(
            // (\W|$) ensure that the keyword is not followed by any alphanumeric char
            r"^(?P<token>fn|if|let|else|return|while)(\W|$)"
        ).unwrap();
        static ref NUMERIC_PATTERN: Regex = Regex::new(
            // (\W|$) ensure that the float is not followed by any alphanumeric char
            r"^(?P<token>\d+(\.\d*)?|\d+\.?|\.\d+)(\W|$)"
        ).unwrap();
        static ref SEPARATOR_PATTERN: Regex = Regex::new(
            // 2chars tokens must come first
            r"^(?P<token>\}|\)|\{|\(|;|,)"
        ).unwrap();
        static ref OPERATOR_PATTERN: Regex = Regex::new(
            // 2chars tokens must come first
            r"^(?P<token>==|>=|<=|!=|\*\*|\+|&|=|/|>|<|%|\*|!|\||\-)"
        ).unwrap();
        static ref STRING_LITERAL_PATTERN: Regex = Regex::new(
            r#"^"(?P<token>(\\"|.)*?)""#
        ).unwrap();
        static ref COMMENT_PATTERN: Regex = Regex::new(
            // use `(?m)` to match until \n
            r"(?m)^//(?P<token>.+?)$"
        ).unwrap();
        static ref IDENTIFIER_PATTERN: Regex = Regex::new(
            r"^(?P<token>[[:alpha:]]\w*)"
        ).unwrap();
    }

    // use `cursor` to keep track of each token position into the original src
    let mut cursor = SrcCursor::new();
    let mut tokens: Vec<Token> = Vec::new();
    // walk through src and build token on the fly
    'token_loop: while let Some(remaining_src) = src.get(cursor.idx..) {
        // quit if all the source has been lexed
        if remaining_src.is_empty() {
            break;
        }
        // match new lines, update cursor only
        if let Some(caps) = NEW_LINE_PATTERN.captures(remaining_src) {
            let value = caps.name("token").unwrap().as_str();
            cursor.consume_n(value.len());
            cursor.new_line();
            continue 'token_loop;
        }
        // match white spaces, update cursor only
        if let Some(caps) = WHITE_SPACE_PATTERN.captures(remaining_src) {
            let value = caps.name("token").unwrap().as_str();
            cursor.consume_n(value.len());
            continue 'token_loop;
        }
        // match string literals
        if let Some(caps) = STRING_LITERAL_PATTERN.captures(remaining_src) {
            let value_str = caps.name("token").unwrap().as_str();
            tokens.push(
                Token::new(
                    Literal(Str(value_str.into())),
                    &cursor
                )
            );
            //consume the matched group AND the 2 `"` chars
            cursor.consume_n(value_str.len() + 2);
            continue 'token_loop;
        }
        // match boolean literals
        if let Some(caps) = BOOLEAN_PATTERN.captures(remaining_src) {
            let value = caps.name("token").unwrap().as_str();
            tokens.push(
                Token::new(
                    Literal(Boolean(value == "true")),
                    &cursor
                )
            );
            cursor.consume_n(value.len());
            continue 'token_loop;
        }
        // match comments
        // (must be done before Operators otherwise it will be interpreted as two divs)
        if let Some(caps) = COMMENT_PATTERN.captures(remaining_src) {
            let comment_content = caps.name("token").unwrap().as_str();
            tokens.push( Token::new(Comment(comment_content.into()), &cursor));
            // consume the 2 '/' as well
            cursor.consume_n(comment_content.len() + 2 );
            continue 'token_loop;
        }
        // match keywords
        if let Some(caps) = KEYWORD_PATTERN.captures(remaining_src) {
            let keyword_str: &str = caps.name("token").unwrap().as_str();
            let kind = match keyword_str {
                "fn" => Function,
                "if" => If,
                "let" => Let,
                "return" => Return,
                "else" => Else,
                "while" => While,
                kw => {
                    return Err(Box::new(
                        LexingError::new(
                            format!("unexpected keyword \"{}\"", kw),
                            &cursor
                        ))
                    );
                },
            };
            tokens.push(Token::new(Keyword(kind), &cursor));
            cursor.consume_n(keyword_str.len());
            continue 'token_loop;
        }
        // match separators
        if let Some(caps) = SEPARATOR_PATTERN.captures(remaining_src) {
            let separator_str: &str = caps.name("token").unwrap().as_str();
            let kind = match separator_str {
                "}" => CloseBlock,
                ")" => CloseParen,
                "{" => OpenBlock,
                "(" => OpenParen,
                ";" => Semicolon,
                "," => Comma,
                punctuator => {
                    return Err(Box::new(
                        LexingError::new(
                            format!("unexpected separator \"{}\"", &separator_str),
                            &cursor
                        ))
                    );
                },
            };
            tokens.push(Token::new(Separator(kind), &cursor));
            cursor.consume_n(separator_str.len());
            continue 'token_loop;
        }
        // match puntuation
        if let Some(caps) = OPERATOR_PATTERN.captures(remaining_src) {
            let operator_str: &str = caps.name("token").unwrap().as_str();
            let kind = match operator_str {
                "+" => Add,
                "&" => And,
                "=" => Assign,
                "/" => Div,
                "==" => Equal,
                ">" => GreaterThan,
                ">=" => GreaterThanOrEq,
                "<" => LessThan,
                "<=" => LessThanOrEq,
                "%" => Mod,
                "*" => Mul,
                "!" => Not,
                "!=" => NotEq,
                "|" => Or,
                "**" => Pow,
                "-" => Sub,
                operator => {
                    return Err(Box::new(
                        LexingError::new(
                            format!("unexpected operator \"{}\"", &operator),
                            &cursor
                        ))
                    );
                },
            };
            tokens.push( Token::new(Operator(kind), &cursor));
            cursor.consume_n(operator_str.len());
            continue 'token_loop;
        }
        // match numeric literals
        if let Some(caps) = NUMERIC_PATTERN.captures(remaining_src) {
            let number_str: &str = caps.name("token").unwrap().as_str();
            match number_str.parse::<f64>() {
                Ok(number) => {
                    tokens.push(Token::new(Literal(Numeric(number)), &cursor));
                },
                Err(_) => {
                    return Err(Box::new(
                        LexingError::new(
                            format!("bad number \"{}\"", &number_str),
                            &cursor
                        ))
                    );
                }
            }
            cursor.consume_n(number_str.len());
            continue 'token_loop;
        }
        // match identifier
        if let Some(caps) = IDENTIFIER_PATTERN.captures(remaining_src) {
            let identifier_str = caps.name("token").unwrap().as_str();
            tokens.push( Token::new(Identifier(identifier_str.into()), &cursor));
            cursor.consume_n(identifier_str.len());
            continue 'token_loop;
        }
        return Err(Box::new(
            LexingError::new(
                format!("syntax error: \"{}\"", &remaining_src),
                &cursor
            ))
        );
    }
    Ok(tokens)
}

mod test {
    use crate::tokens::SeparatorKind::*;
    use crate::tokens::TokenKind::*;
    use crate::tokens::OperatorKind::*;
    use crate::tokens::LiteralKind::*;
    use crate::tokens::KeywordKind::*;
    use crate::lexer::lex;

    #[test]
    fn lex_literals() {
        // boolean
        let r = lex("true").unwrap();
        assert_eq!(r[0].kind, Literal(Boolean(true)));
        let r = lex("false").unwrap();
        assert_eq!(r[0].kind, Literal(Boolean(false)));

        // numeric
        let r = lex("1").unwrap();
        assert_eq!(r[0].kind, Literal(Numeric(1.)));
        let r = lex("-1").unwrap();
        assert_eq!(r[0].kind, Operator(Sub));
        assert_eq!(r[1].kind, Literal(Numeric(1.)));
        let r = lex("-1.34").unwrap();
        assert_eq!(r[0].kind, Operator(Sub));
        assert_eq!(r[1].kind, Literal(Numeric(1.34)));
        let r = lex("-.34").unwrap();
        assert_eq!(r[0].kind, Operator(Sub));
        assert_eq!(r[1].kind, Literal(Numeric(0.34)));
        let r = lex(".");
        assert!(r.is_err());
        // don't overlap with other patterns
        let r = lex("1bim");
        assert!(r.is_err());
        
        // String litterals
        let r = lex(r#" "true" "#).unwrap();
        assert_eq!(r[0].kind, Literal(Str("true".into())));
        // test escaped content
        let r = lex(r#" "\"escaped\"" "#).unwrap();
        assert_eq!(r[0].kind, Literal(Str(r#"\"escaped\""#.into())));
        let r = lex(r#" \"str" "#);
        assert!(r.is_err());
    }

    #[test]
    fn lex_keyword() {
        let r = lex("while").expect("failed to parse keyword.");
        assert_eq!(
            r[0].kind,
            Keyword(While)
        );
        let r = lex("forage").expect("failed to parse keyword.");
        assert_eq!(
            r[0].kind,
            Identifier("forage".into())
        );
    }

    #[test]
    fn lex_separator() {
        let r = lex("{0}").unwrap();
        assert_eq!(r[0].kind, Separator(OpenBlock));
        assert_eq!(r[2].kind, Separator(CloseBlock));
    }

    #[test]
    fn lex_operator() {
        // test 2 chars operators
        let r = lex(">=").unwrap();
        assert_eq!(r[0].kind, Operator(GreaterThanOrEq));
        let r = lex("=").unwrap();
        assert_eq!(r[0].kind, Operator(Assign));
        let r = lex("+").unwrap();
        assert_eq!(r[0].kind, Operator(Add));
        let r = lex("!").unwrap();
        assert_eq!(r[0].kind, Operator(Not));
    }

    #[test]
    fn lex_identifier() {
        let r = lex("some_name").unwrap();
        assert_eq!(r[0].kind, Identifier("some_name".into()));
        // test single letter identifier
        let r = lex("a = 0;").expect("failed to parse single letter identifier");
        assert_eq!(r[0].kind, Identifier("a".into()));
    }

    #[test]
    fn lex_comments() {
        let r = lex("1; //comment").unwrap();
        assert_eq!(r[2].kind, Comment("comment".into()));
        // test multiline handling
        let r = lex("1; //comment\na = 0;").unwrap();
        assert_eq!(r[2].kind, Comment("comment".into()));
        assert_eq!(r[3].kind, Identifier("a".into()));
    }
}
