use std::error::Error;
//use std::str::FromStr;
use std::fmt;
use crate::rule_lexer::{
    RuleToken,
    LexError,
};


#[derive(Debug,PartialEq,Eq)]
pub struct ParseError(String);
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}.", self.0)
    }
}
impl Error for ParseError {}

#[derive(Debug,PartialEq,Eq,Clone,Copy)]
pub enum TargetKind {
    // targets
    Start,
    End,
    Literal(char),
}

/// A flat representation of an Abstract syntax Tree
#[derive(Debug,PartialEq,Eq)]
pub enum FlatRuleExp {
    /// consumes 1 expressions
    FlatAtLeastOnce,
    /// consumes 1 expressions
    FlatAny,
    /// consumes 1 expressions
    FlatOptional,
    /// consumes `n` expressions, on per variant
    FlatVariants(usize),
    /// consumes 1 expressions
    FlatTarget(TargetKind),
    /// consumes `n` expressions
    FlatSequence(usize),
    /// consumes 0 expressions, delimits expressions
    FlatFence,
}

impl FlatRuleExp {

    /// return the number of sub expressions
    /// that a rule is made of.
    pub fn sub_expression_count(&self) -> usize {
        match *self {
            Self::FlatAtLeastOnce => 1,
            Self::FlatAny => 1,
            Self::FlatOptional => 1,
            Self::FlatVariants(n) => n,
            Self::FlatTarget(_) => 1,
            Self::FlatSequence(n) => n,
            Self::FlatFence => 0,
        }
    }
}

pub trait FlatAst {

    /// Returns all the children node indices and the children of self[n]
    fn sub_exp_indices(&self, n: usize) -> Option<Vec<usize>>;
    /// Returns the next node index in the same level as self[n]
    fn next_exp_in_level(&self, n: usize) -> Option<usize>;
    /// Returns all the direct children indices of self[n]
    fn children_indices(&self, n: usize) -> Option<Vec<usize>>;
}

impl FlatAst for [FlatRuleExp] {

    fn sub_exp_indices(&self, n: usize) -> Option<Vec<usize>> {
        if n > self.len() {
            return None;
        }
        let mut idx = n;
        let mut count = self[n].sub_expression_count();
        let mut indices = Vec::new();
        while count > 0 {
            count += self[idx].sub_expression_count();
            count -= 1;
            idx += 1;
            if idx < self.len() {
                return None;
            }
            indices.push(idx);
        }
        Some(indices)

    }

    fn next_exp_in_level(&self, n: usize) -> Option<usize> {
        if n > self.len() {
            return None;
        }
        let mut idx = n;
        let mut count = self[n].sub_expression_count();
        while count > 0 {
            count += self[idx].sub_expression_count();
            count -= 1;
            idx += 1;
            if idx < self.len() {
                return None;
            }
        }
        Some(idx)
    }

    fn children_indices(&self, n: usize) -> Option<Vec<usize>> {
        if n > self.len() {
            return None;
        }
        match self[n].sub_expression_count() {
            0 => return None,
            children_count => {
                // the child expression must be the next expression
                let mut child_idx = n + 1;
                let mut children_ids: Vec<usize> = vec![child_idx];

                while let Some(idx) = self.next_exp_in_level(child_idx) {
                    child_idx = idx;
                    children_ids.push(idx);
                }
                return Some(children_ids);
            }
        }
    }
}

/// match a quantifier (*, +, ?). must be parsed before its sub-expression
fn match_quantifiers(tokens: &[Option<&RuleToken>]) -> Option<(FlatRuleExp, Vec<usize>)> {
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
                    return Some((FlatRuleExp::FlatAny, vec![idx]));
                } else { break;}
            },
            RuleToken::AtLeastOnce => {
                if expr_count == 1 && parenthesis_count == 0 { 
                    return Some((FlatRuleExp::FlatAtLeastOnce, vec![idx])); 
                } else { break;}
            },
            RuleToken::Optional => {
                if expr_count == 1 && parenthesis_count == 0 { 
                    return Some((FlatRuleExp::FlatOptional, vec![idx]));
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
fn match_parenthesis_fenced(tokens: &[Option<&RuleToken>]) -> Option<(FlatRuleExp, Vec<usize>)> {
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
    Some((FlatRuleExp::FlatFence, parenthesis_indices))
}

/// match variants eg: <expr_a>|<expr_b>|...|<expr_n>
fn match_variants(tokens: &[Option<&RuleToken>]) -> Option<(FlatRuleExp, Vec<usize>)> {
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
    Some((FlatRuleExp::FlatVariants(or_indices.len() + 1), or_indices))
}

/// match a sequence of expressions
fn match_sequence(tokens: &[Option<&RuleToken>], last: Option<&FlatRuleExp>) -> Option<(FlatRuleExp, Vec<usize>)> {
    if let Some(FlatRuleExp::FlatSequence(_)) = last {
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
    Some((FlatRuleExp::FlatSequence(group_nb), vec![]))
}

/// A Target object, e.g: a literal, '^', '$' ...
/// only if it is in the *first* position
fn match_target(tokens: &[Option<&RuleToken>]) -> Option<(FlatRuleExp, Vec<usize>)> {
    match tokens.first() {
        Some(Some(RuleToken::Start)) => Some((FlatRuleExp::FlatTarget(TargetKind::Start), vec![0])),
        Some(Some(RuleToken::End)) => Some((FlatRuleExp::FlatTarget(TargetKind::End), vec![0])),
        Some(Some(RuleToken::Literal(c))) => Some((FlatRuleExp::FlatTarget(TargetKind::Literal(*c)), vec![0])),
        _ => None,
    }
}

/// Parse a token slice into a flat representation 
/// of a Rule Abstract Syntax Tree, in Reverse Polish Notation
pub fn parse_flat_rules(tokens: &[RuleToken]) -> Result<Vec<FlatRuleExp>, ParseError> {
    let mut exprs: Vec<FlatRuleExp> = Vec::new();
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
            FlatRuleExp::FlatFence => {}, // filter out fences
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


#[derive(Debug,PartialEq,Eq)]
pub enum RuleExp {
    // repetitions
    Once(Box<RuleExp>),
    AtLeastOnce(Box<RuleExp>),
    Any(Box<RuleExp>),
    Optional(Box<RuleExp>),
    // Combinators
    Variants(Vec<RuleExp>),
    // todo: Class
    Target(TargetKind),
    // 
    Sequence(Vec<RuleExp>),
}

/// This stuct represent a rule **Abstract syntax Tree**.
/// It can be use to build a DFA or NFA
#[derive(Debug,PartialEq,Eq)]
pub struct RuleAst {
    pub rules: RuleExp,
}

impl RuleAst {

    /// Build a `RuleAst` from a vector of flat expressions.
    fn from_flat_rule_expressions(mut flat_exps: Vec<FlatRuleExp>) -> Result<Self,ParseError> {
        use FlatRuleExp::*;
        use RuleExp::*;
        let mut expressions: Vec<RuleExp> = Vec::new();
        while let Some(flat_exp) = flat_exps.pop() {
            dbg!(&flat_exp);
            match flat_exp {
                FlatSequence(nb) => {
                    let mut sequence: Vec<RuleExp> = Vec::new();
                    for _ in 0..nb {
                        sequence.push(
                            expressions
                                .pop()
                                .ok_or(ParseError("No expression to pop.".into()))?
                        );
                    }
                    expressions.push(Sequence(sequence));
                },
                FlatTarget(target) => {
                    expressions.push(Target(target));
                },
                FlatVariants(nb) => {
                    let mut variants: Vec<RuleExp> = Vec::new();
                    for _ in 0..nb {
                        variants.push(
                            expressions
                                .pop()
                                .ok_or(ParseError("No expression to pop.".into()))?
                        );
                    }
                    expressions.push(Variants(variants));
                },
                FlatOptional => {
                    let optional_exp = expressions
                        .pop()
                        .ok_or(ParseError("No expression to pop.".into()))?;
                    expressions.push(Optional(Box::new(optional_exp)));
                },
                FlatAny => {
                    let exp = expressions
                        .pop()
                        .ok_or(ParseError("No expression to pop.".into()))?;
                    expressions.push(Any(Box::new(exp)));
                },
                FlatAtLeastOnce => {
                    let exp = expressions
                        .pop()
                        .ok_or(ParseError("No expression to pop.".into()))?;
                    expressions.push(AtLeastOnce(Box::new(exp)));
                },
                FlatFence => {},
            }
        }
        Ok(RuleAst { rules: expressions.pop().ok_or(ParseError("expressions list is empty".into()))?})
    }

    /// Build a `RuleAst` from a rule/pattern string
    pub fn from_str(rule_str: &str) -> Result<Self, ParseError> {
        let tokens: Vec<RuleToken> = RuleToken::from_str(rule_str)
            .map_err(|_| ParseError("Failed to lex the input str".into()))?;
        let rules = parse_flat_rules(&tokens)?;
        RuleAst::from_flat_rule_expressions(rules)
    }
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
            FlatRuleExp::FlatOptional,
            FlatRuleExp::FlatTarget(TargetKind::Literal('c')),
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
            FlatRuleExp::FlatSequence(2),
            FlatRuleExp::FlatTarget(TargetKind::Literal('a')),
            FlatRuleExp::FlatOptional,
            FlatRuleExp::FlatTarget(TargetKind::Literal('b')),
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
            FlatRuleExp::FlatVariants(2),
            FlatRuleExp::FlatTarget(TargetKind::Literal('a')),
            FlatRuleExp::FlatOptional,
            FlatRuleExp::FlatTarget(TargetKind::Literal('b')),
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
            FlatRuleExp::FlatAny,
            FlatRuleExp::FlatVariants(2),
            FlatRuleExp::FlatTarget(TargetKind::Literal('a')),
            FlatRuleExp::FlatTarget(TargetKind::Literal('b')),
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
            FlatRuleExp::FlatSequence(2),
            FlatRuleExp::FlatTarget(TargetKind::Literal('c')),
            FlatRuleExp::FlatAny,
            FlatRuleExp::FlatVariants(2),
            FlatRuleExp::FlatSequence(2),
            FlatRuleExp::FlatTarget(TargetKind::Literal('a')), 
            FlatRuleExp::FlatTarget(TargetKind::Literal('a')), 
            FlatRuleExp::FlatTarget(TargetKind::Literal('b')),
        ]),
    );
}

#[test]
fn ast_from_flat_expressions() {
    // c(aa|b)*
    let flat_exprs = vec![
        FlatRuleExp::FlatSequence(2),
        FlatRuleExp::FlatTarget(TargetKind::Literal('c')),
        FlatRuleExp::FlatAny,
        FlatRuleExp::FlatVariants(2),
        FlatRuleExp::FlatSequence(2),
        FlatRuleExp::FlatTarget(TargetKind::Literal('a')), 
        FlatRuleExp::FlatTarget(TargetKind::Literal('a')), 
        FlatRuleExp::FlatTarget(TargetKind::Literal('b')),
    ];
    use RuleExp::*;
    use TargetKind::*;
    assert_eq!(
        RuleAst::from_flat_rule_expressions(flat_exprs),
        Ok( 
            RuleAst {
                rules: Sequence(
                    vec![
                        Target(Literal('c')), 
                        Any(Box::new(
                            Variants(vec![
                                Sequence(
                                    vec![
                                        Target(Literal('a')), 
                                        Target(Literal('a'))
                                    ]
                                ),
                                Target(Literal('b')),
                            ])
                        )),
                    ]
                )
            }
        )
    );
}

#[test]
fn ast_from_str() {
    // a|bc
    use RuleExp::*;
    use TargetKind::*;
    assert_eq!(
        RuleAst::from_str("a|bc"),
        Ok( 
            RuleAst {
                rules: Variants(
                           vec![
                                Target(Literal('a')),
                                Sequence(
                                    vec![
                                        Target(Literal('b')), 
                                        Target(Literal('c'))
                                    ]
                                ),
                            ]
                        )
            }
        )
    );
    // c(aa|b)*
    assert_eq!(
        RuleAst::from_str("c(aa|b)*"),
        Ok( 
            RuleAst {
                rules: Sequence(
                    vec![
                        Target(Literal('c')), 
                        Any(Box::new(
                            Variants(vec![
                                Sequence(
                                    vec![
                                        Target(Literal('a')), 
                                        Target(Literal('a'))
                                    ]
                                ),
                                Target(Literal('b')),
                            ])
                        )),
                    ]
                )
            }
        )
    );
}
