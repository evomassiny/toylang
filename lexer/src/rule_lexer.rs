use std::error::Error;
use std::fmt;

#[derive(Debug,PartialEq,Eq)]
pub struct LexError(usize, String);
impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Lexing error at char {}: {}.", self.0, self.1)
    }
}
impl Error for LexError {}

#[derive(Debug,PartialEq,Eq)]
pub enum RuleToken {
    // zero length match
    Start,              // ^
    End,                // $
    // conditions
    Or,                 // |
    // groupers
    OpenParen,          // (
    CloseParen,         // )
    // quantifiers
    Any,                // *
    AtLeastOnce,        // +
    Optional,           // ?
    // char classes: TODO
    // Characters
    Literal(char),      // any char
}

/// lex a Rule (regex syntax subset)
/// from a str to a vector of `RuleToken`
fn from_str(src: &str) -> Result<Vec<RuleToken>,LexError> {
    let mut tokens = Vec::new();

    let mut chars = src.char_indices().peekable();
    while let Some((idx, c)) = chars.next() {
        let token = match c {
            // zero length
            '^' => RuleToken::Start,
            '$' => RuleToken::End,
            // condition
            '|' => RuleToken::Or,
            // groupers
            '(' => RuleToken::OpenParen,
            ')' => RuleToken::CloseParen,
            // quantifiers
            '*' => RuleToken::Any,
            '+' => RuleToken::AtLeastOnce,
            '?' => RuleToken::Optional,
            // characters
            '\\' => {
                // handle escaped chars
                let token = match chars.peek() {
                    Some((_, next_c)) => {
                        match *next_c {
                            // escape secial chars
                            '^' | '$' | '|' | '(' | ')' | '*' | '+' | '?' | '\\' => RuleToken::Literal(*next_c),
                            // nothing to escape, fails
                            _ => return Err(LexError(idx, format!("cannot escape {}", next_c))),
                        }
                    },
                    None => return Err(LexError(idx, format!("invalid escape char")) ),
                };
                // consumme next char
                let _ = chars.next();
                token
            },
            literal => RuleToken::Literal(literal),
        };
        tokens.push(token);
    }
    Ok(tokens)
}

#[test]
fn lex_zero_length() {
    assert_eq!(
        from_str(""),
        Ok(vec![]),
    );
    assert_eq!(
        from_str("^"),
        Ok(vec![RuleToken::Start]),
    );
    assert_eq!(
        from_str("$"),
        Ok(vec![RuleToken::End]),
    );
}

#[test]
fn lex_conditions() {
    assert_eq!(
        from_str("|"),
        Ok(vec![RuleToken::Or]),
    );
}

#[test]
fn lex_groupers() {
    assert_eq!(
        from_str("("),
        Ok(vec![RuleToken::OpenParen]),
    );
    assert_eq!(
        from_str(")"),
        Ok(vec![RuleToken::CloseParen]),
    );
}

#[test]
fn lex_quantifiers() {
    assert_eq!(
        from_str("*"),
        Ok(vec![RuleToken::Any]),
    );
    assert_eq!(
        from_str("+"),
        Ok(vec![RuleToken::AtLeastOnce]),
    );
    assert_eq!(
        from_str("?"),
        Ok(vec![RuleToken::Optional]),
    );
}

#[test]
fn lex_literals() {
    assert_eq!(
        from_str("ab"),
        Ok(vec![
            RuleToken::Literal('a'),
            RuleToken::Literal('b'),
        ]),
    );
    assert!(from_str(r"\a").is_err());
    assert!(from_str(r"\").is_err());

    assert_eq!(
        from_str(r"\^\$\|\(\)\*\+\?\\"),
        Ok(vec![
            RuleToken::Literal('^'),
            RuleToken::Literal('$'),
            RuleToken::Literal('|'),
            RuleToken::Literal('('),
            RuleToken::Literal(')'),
            RuleToken::Literal('*'),
            RuleToken::Literal('+'),
            RuleToken::Literal('?'),
            RuleToken::Literal('\\'),
        ]),
    );
}

#[test]
fn lex_mixed() {
    assert_eq!(
        from_str("^ab?(a|c)$"),
        Ok(vec![
            RuleToken::Start,
            RuleToken::Literal('a'),
            RuleToken::Literal('b'),
            RuleToken::Optional,
            RuleToken::OpenParen,
            RuleToken::Literal('a'),
            RuleToken::Or,
            RuleToken::Literal('c'),
            RuleToken::CloseParen,
            RuleToken::End,
        ]),
    );
}
