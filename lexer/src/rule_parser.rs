use crate::rule_ast::{
    RuleExp,
    RuleAst,
    TargetKind,
};
use crate::rule_lexer::{
    RuleToken,
    LexError,
};
use std::fmt;
use std::error::Error;

#[derive(Debug,PartialEq,Eq)]
pub struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}.", self.0)
    }
}
impl Error for ParseError {}
impl ParseError {
    pub fn new(msg: &str) -> ParseError {
        Self(msg.into())
    }
}


/// match a quantifier (*, +, ?). must be parsed before its sub-expression
fn match_quantifiers(tokens: &[Option<&RuleToken>]) -> Option<(RuleExp, Vec<usize>)> {
    let mut idx: usize = 0;
    let mut parenthesis_count: isize = 0;
    let mut expr_count: isize = 0;
    while let Some(Some(token)) = tokens.get(idx) {
        match token {
            // do not enter parenthesis, 
            RuleToken::OpenParen => {
                if parenthesis_count == 0 { expr_count += 1; }
                parenthesis_count += 1;
            },
            RuleToken::CloseParen => parenthesis_count -= 1,
            RuleToken::Start | RuleToken::End | RuleToken::Literal(_) => {
                if parenthesis_count == 0 {
                    expr_count += 1;
                }
            },
            RuleToken::Any => {
                if expr_count == 1 && parenthesis_count == 0 { 
                    return Some((RuleExp::Any, vec![idx]));
                } else { break;}
            },
            RuleToken::AtLeastOnce => {
                if expr_count == 1 && parenthesis_count == 0 { 
                    return Some((RuleExp::AtLeastOnce, vec![idx])); 
                } else { break;}
            },
            RuleToken::Optional => {
                if expr_count == 1 && parenthesis_count == 0 { 
                    return Some((RuleExp::Optional, vec![idx]));
                } else { break;}
            },
            _ => {},
        }
        if expr_count > 1 { 
            break; 
        }
        idx += 1;
    }
    None
}

/// Match a parenthesis group **starting at the index 0**
fn match_parenthesis_fenced(tokens: &[Option<&RuleToken>]) -> Option<(RuleExp, Vec<usize>)> {
    let mut idx: usize = 0;
    let mut parenthesis_count: isize = 0;
    let mut parenthesis_indices: Vec<usize> = Vec::new();
    while let Some(token) = tokens.get(idx) {
        match *token {
            Some(RuleToken::OpenParen) => {
                if parenthesis_count == 0 {
                    if idx > 0 { return None; }
                    parenthesis_indices.push(idx);
                }
                parenthesis_count += 1;
            },
            Some(RuleToken::CloseParen) => {
                parenthesis_count -= 1;
                if parenthesis_count == 0 {
                    parenthesis_indices.push(idx);
                }
            },
            None => break,
            _ => {},
        }
        idx += 1;
    }
    if parenthesis_indices.is_empty() {
        return None;
    }
    Some((RuleExp::Fence, parenthesis_indices))
}

/// match variants eg: <expr_a>|<expr_b>|...|<expr_n>
fn match_variants(tokens: &[Option<&RuleToken>]) -> Option<(RuleExp, Vec<usize>)> {
    let mut idx: usize = 0;
    let mut parenthesis_count: isize = 0;
    let mut or_indices: Vec<usize> = Vec::new();
    while let Some(token) = tokens.get(idx) {
        match *token {
            // do not enter parenthesis, 
            Some(RuleToken::OpenParen) => parenthesis_count += 1,
            Some(RuleToken::CloseParen) => parenthesis_count -= 1,
            Some(RuleToken::Or) => {
                // only store the variants at the lowest level
                if parenthesis_count == 0 {
                    or_indices.push(idx);
                }
            }
            None => break,
            _ => {},
        }
        idx += 1;
    }
    if or_indices.is_empty() {
        return None;
    }
    Some((RuleExp::Variants(or_indices.len() + 1), or_indices))
}

/// match a sequence of expressions
fn match_sequence(tokens: &[Option<&RuleToken>], last: Option<&RuleExp>) -> Option<(RuleExp, Vec<usize>)> {
    if let Some(RuleExp::Sequence(_)) = last {
        // cannot have 2 sequences in a row
        // because a matched sequence does not consume any Token,
        // this check avoid infinite loops
        return None;
    }
    let mut idx: usize = 0;
    let mut parenthesis_count: isize = 0;
    let mut group_nb: usize = 0;
    use RuleToken::*;
    while let Some(token) = tokens.get(idx) {
        match *token {
            Some(OpenParen) => parenthesis_count += 1,
            Some(CloseParen) => {
                parenthesis_count -= 1;
                if parenthesis_count == 0 {
                    group_nb += 1;
                }
            },
            Some(Start) | Some(End) | Some(Literal(_)) => {
                if parenthesis_count == 0 {
                    group_nb += 1;
                }
            },
            None => break,
            _ => {},
        }
        idx += 1;
    }
    if group_nb <= 1 {
        return None;
    }
    Some((RuleExp::Sequence(group_nb), vec![]))
}

/// A Target object, e.g: a literal, '^', '$' ...
/// only if it is in the *first* position
fn match_target(tokens: &[Option<&RuleToken>]) -> Option<(RuleExp, Vec<usize>)> {
    match tokens.first() {
        Some(Some(RuleToken::Start)) => Some((RuleExp::Target(TargetKind::Start), vec![0])),
        Some(Some(RuleToken::End)) => Some((RuleExp::Target(TargetKind::End), vec![0])),
        Some(Some(RuleToken::Literal(c))) => Some((RuleExp::Target(TargetKind::Literal(*c)), vec![0])),
        _ => None,
    }
}

/// Parse a token slice into a flat representation 
/// of a Rule Abstract Syntax Tree, in Reverse Polish Notation
pub fn parse_flat_rules(tokens: &[RuleToken]) -> Result<Vec<RuleExp>, ParseError> {
    let mut exprs: Vec<RuleExp> = Vec::new();
    // in this vector, a None item represents a parsed RuleToken
    let mut unparsed_tokens: Vec<Option<&RuleToken>> = tokens.iter()
        .map(Some) // as Option<&RuleToken>
        .collect();
    let mut idx: usize = 0;
    let tokens_len = unparsed_tokens.len();
    while idx < tokens_len {
        // only increment when the current token is parsed
        if unparsed_tokens[idx].is_none() {
            idx +=1;
            continue;
        }

        // match expresions patterns, order matters.
        let (flat_exp, parsed_tokens) =
                 if let Some((exp, parsed)) = match_variants(&unparsed_tokens[idx..]) { (exp, parsed) }
            else if let Some((exp, parsed)) = match_sequence(&unparsed_tokens[idx..], exprs.last()) { (exp, parsed) }
            else if let Some((exp, parsed)) = match_quantifiers(&unparsed_tokens[idx..]) { (exp, parsed) }
            else if let Some((exp, parsed)) = match_parenthesis_fenced(&unparsed_tokens[idx..]) { (exp, parsed) }
            else if let Some((exp, parsed)) = match_target(&unparsed_tokens[idx..]) { (exp, parsed) }
            else { 
                return Err(ParseError(format!("unexpected end at token {}, {:?}", &idx, &unparsed_tokens[idx])));
            };

        // store parsed expression into a flat tree
        match flat_exp {
            RuleExp::Fence => {}, // filter out fences
            e => {
                exprs.push(e);
            }
        }

        // consume parsed tokens
        for i in parsed_tokens {
            unparsed_tokens[idx + i] = None;
        }
    }
    Ok(exprs)
}

#[test]
fn flat_quantifier_parse() {
    // c?
    let tokens = [
        RuleToken::Literal('c'),
        RuleToken::Optional,
    ];
    assert_eq!(
        parse_flat_rules(&tokens),
        Ok(vec![
            RuleExp::Optional,
            RuleExp::Target(TargetKind::Literal('c')),
        ]),
    );
    // ab?
    let tokens = [
        RuleToken::Literal('a'),
        RuleToken::Literal('b'),
        RuleToken::Optional,
    ];
    assert_eq!(
        parse_flat_rules(&tokens),
        Ok(vec![
            RuleExp::Sequence(2),
            RuleExp::Target(TargetKind::Literal('a')),
            RuleExp::Optional,
            RuleExp::Target(TargetKind::Literal('b')),
        ]),
    );
    // a|b?
    let tokens = [
        RuleToken::Literal('a'),
        RuleToken::Or,
        RuleToken::Literal('b'),
        RuleToken::Optional,
    ];
    assert_eq!(
        parse_flat_rules(&tokens),
        Ok(vec![
            RuleExp::Variants(2),
            RuleExp::Target(TargetKind::Literal('a')),
            RuleExp::Optional,
            RuleExp::Target(TargetKind::Literal('b')),
        ]),
    );
    // (a|b)*
    let tokens = [
        RuleToken::OpenParen,
        RuleToken::Literal('a'),
        RuleToken::Or,
        RuleToken::Literal('b'),
        RuleToken::CloseParen,
        RuleToken::Any,
    ];
    assert_eq!(
        parse_flat_rules(&tokens),
        Ok(vec![
            RuleExp::Any,
            RuleExp::Variants(2),
            RuleExp::Target(TargetKind::Literal('a')),
            RuleExp::Target(TargetKind::Literal('b')),
        ]),
    );
}

#[test]
fn flat_parse() {
    // c(aa|b)*
    let tokens = [
        RuleToken::Literal('c'),
        RuleToken::OpenParen,
        RuleToken::Literal('a'),
        RuleToken::Literal('a'),
        RuleToken::Or,
        RuleToken::Literal('b'),
        RuleToken::CloseParen,
        RuleToken::Any,
    ];
    assert_eq!(
        parse_flat_rules(&tokens),
        Ok(vec![
            RuleExp::Sequence(2),
            RuleExp::Target(TargetKind::Literal('c')),
            RuleExp::Any,
            RuleExp::Variants(2),
            RuleExp::Sequence(2),
            RuleExp::Target(TargetKind::Literal('a')), 
            RuleExp::Target(TargetKind::Literal('a')), 
            RuleExp::Target(TargetKind::Literal('b')),
        ]),
    );
}
