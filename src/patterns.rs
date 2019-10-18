use crate::parser::{
    FlatExp,
    FlatExp::*,
};
use crate::tokens::{
    TokenKind::*,
    Token,
    LiteralKind::*,
    SeparatorKind::*,
    OperatorKind::*,
    KeywordKind::*,
    SrcCursor
};

/// return the index of the last CloseBlock token of a block
fn consume_block(tokens: &[Option<&Token>]) -> Option<usize> {
    if tokens.len() < 2 { return None; }
    if let Some(&Token { kind: Separator(OpenBlock), ..}) = tokens[0] {
        let mut block_counter = 1;
        for i in 1..tokens.len() {
            match tokens[i] {
                Some(&Token { kind: Separator(OpenBlock), ..}) => {
                    block_counter += 1;
                },
                Some(&Token { kind: Separator(CloseBlock), ..}) => {
                    block_counter -= 1;
                    if block_counter == 0 {
                        return Some(i);
                    }
                },
                None => return None,
                _ => continue,
            }
        }
    }
    None
}

pub fn match_binary_op(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    // todo
    None
}
pub fn match_unary_op(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    // todo
    None
}
pub fn match_const(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    // todo
    None
}

/// match a block starting at the 1st item of `token`
pub fn match_block(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 2 { return None; }
    // if the first token match a '{',
    // iter the token slice until we find the correct '}'
    // and count the number of sub expressions (either statements, or nested blocks)
    if let Some(&Token { kind: Separator(OpenBlock), ..}) = tokens[0] {
        let mut subexpr = 0;
        let mut block_counter = 1;
        let mut consumed_tokens =  vec![0];
        for i in 1..tokens.len() {
            match tokens[i] {
                Some(&Token { kind: Separator(OpenBlock), ..}) => {
                    block_counter += 1;
                    subexpr +=1;
                },
                Some(&Token { kind: Separator(CloseBlock), ..}) => {
                    block_counter -= 1;
                    if block_counter == 0 {
                        consumed_tokens.push(i);
                        return Some((FlatBlock(subexpr), consumed_tokens));
                    }
                },
                Some(&Token { kind: Separator(Semicolon), ..}) => {
                    subexpr +=1;
                }
                None => return None,
                _ => continue,
            }
        }
    }
    None
}

/// match a function call starting at the 1st item of `token`
pub fn match_call(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 3 { return None; }
    // FlatCall(String, usize),
    if let Some(&Token { kind: Identifier(ref id), ..}) = tokens[0] {
        if let Some(&Token { kind: Separator(OpenParen), ..}) = tokens[1] {
            let mut subexpr = 0;
            let mut paren_counter = 1;
            let mut consumed_tokens =  vec![0, 1];

            for i in 2..tokens.len() {
                match tokens[i] {
                    Some(&Token { kind: Separator(OpenParen), ..}) => {
                        paren_counter += 1;
                    },
                    
                    Some(&Token { kind: Separator(CloseParen), ..}) => {
                        paren_counter -= 1;
                        if paren_counter == 0 {
                            consumed_tokens.push(i);
                            if subexpr > 0 { subexpr += 1; } // one comma means 2 args
                            return Some((FlatCall(id.into(), subexpr), consumed_tokens));
                        }
                    },
                    Some(&Token { kind: Separator(Comma), ..}) => {
                        subexpr +=1;
                        consumed_tokens.push(i);
                    },
                    None => return None,
                    _ => continue,
                }
            }
        }
    }
    None
}
/// match (starting from the 1st token):
/// * if (expression) {block}
/// * if (expression) {block} else {block}
pub fn match_if(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 4 { return None; }
    if let Some(&Token { kind: Keyword(If), ..}) = tokens[0] {
        if let Some(&Token { kind: Separator(OpenParen), ..}) = tokens[1] {
            let mut paren_counter = 1;
            let mut consumed_tokens =  vec![0, 1];

            // match parenthesis after `if` and consume them
            let mut i = 1;
            while i < tokens.len() {
                i += 1;
                match tokens[i] {
                    Some(&Token { kind: Separator(OpenParen), ..}) => {
                        paren_counter += 1;
                    },
                    Some(&Token { kind: Separator(CloseParen), ..}) => {
                        paren_counter -= 1;
                        if paren_counter == 0 { 
                            consumed_tokens.push(i);
                            break;
                        }
                    },
                    None => return None,
                    _ => continue,
                }
            }
            i += 1; // go after CloseParen
            i += consume_block(&tokens[i..])?; // go at CloseBlock
            // if the if is followed by an else:
            if tokens[i..].len() > 3 {
                i += 1; // go after CloseBlock
                if let Some(&Token { kind: Keyword(Else), ..}) = tokens[i] {
                    consumed_tokens.push(i);
                    i += 1; // go after 'else'
                    let _ = consume_block(&tokens[i..])?;
                    return Some((FlatIf(3), consumed_tokens));
                }
            }
            // simple if without else
            return Some((FlatIf(2), consumed_tokens));
        }
    }
    None
}

pub fn match_while(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 4 { return None; }
    if let Some(&Token { kind: Keyword(While), ..}) = tokens[0] {
        if let Some(&Token { kind: Separator(OpenParen), ..}) = tokens[1] {
            let mut paren_counter = 1;
            let mut consumed_tokens =  vec![0, 1];

            // match parenthesis after `if` and consume them
            let mut i = 1;
            while i < tokens.len() {
                i += 1;
                match tokens[i] {
                    Some(&Token { kind: Separator(OpenParen), ..}) => {
                        paren_counter += 1;
                    },
                    Some(&Token { kind: Separator(CloseParen), ..}) => {
                        paren_counter -= 1;
                        if paren_counter == 0 { 
                            consumed_tokens.push(i);
                            break;
                        }
                    },
                    None => return None,
                    _ => continue,
                }
            }
            i += 1; // go after CloseParen
            i += consume_block(&tokens[i..])?; // go at CloseBlock
            return Some((FlatWhileLoop, consumed_tokens));
        }
    }
    None
}

pub fn match_function_decl(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    // todo
    None
}
pub fn match_return(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    // todo
    None
}
pub fn match_assign(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    // todo
    None
}
pub fn match_let(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    // todo
    None
}

mod test {
    use crate::patterns;
    use crate::tokens::{
        TokenKind,
        Token,
        LiteralKind,
        SeparatorKind,
        OperatorKind,
        KeywordKind,
        SrcCursor
    };
    use crate::lexer::lex;
    use crate::parser::FlatExp;

    #[test]
    fn match_block_pattern() {
        // test single block
        let tokens = lex("{ let A = 1; }").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_block(&unparsed_tokens),
            Some((FlatExp::FlatBlock(1), vec![0_usize, 6])),
            "Failed to match block pattern"
        );
        // test 2 blocks
        let tokens = lex("{{ let A = 1; }}").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_block(&unparsed_tokens),
            Some((FlatExp::FlatBlock(2), vec![0_usize, 8])),
            "Failed to match block pattern"
        );
        // test unclosed block
        let tokens = lex("{{ let A = 1; }").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_block(&unparsed_tokens),
            None,
        );
    }

    #[test]
    fn match_function_call_pattern() {
        // test simple call
        let tokens = lex("call()").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_call(&unparsed_tokens),
            Some((FlatExp::FlatCall("call".into(), 0), vec![0_usize, 1, 2])),
            "Failed to match function call pattern"
        );
        // test call with arguments
        let tokens = lex("call(1,(1+3))").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_call(&unparsed_tokens),
            Some((FlatExp::FlatCall("call".into(), 2), vec![0_usize, 1, 3, 9])),
            "Failed to match function call pattern"
        );
    }

    #[test]
    fn match_if_pattern() {
        // test simple call
        let tokens = lex(r#"
        if ( true ) {
            a = 1;
        }
        "#).unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_if(&unparsed_tokens),
            Some((FlatExp::FlatIf(2), vec![0_usize, 1, 3])),
            "Failed to match if pattern"
        );
        let tokens = lex(r#"
        if ( true ) {
            a = 1;
        } else {
            a = 2;
        }
        "#).unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_if(&unparsed_tokens),
            Some((FlatExp::FlatIf(3), vec![0_usize, 1, 3, 10])),
            "Failed to match if pattern"
        );
    }

    #[test]
    fn match_while_pattern() {
        // test simple while
        let tokens = lex(r#"
        while ( a > 6 ) {
            a = a + 1;
        }
        "#).unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_while(&unparsed_tokens),
            Some((FlatExp::FlatWhileLoop, vec![0_usize, 1, 5])),
            "Failed to match while pattern"
        );

        // test invalid while
        let tokens = lex(r#"
        while ( a > 6 ) {
            a = a + 1;
        "#).unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_while(&unparsed_tokens),
            None,
            "Should hanve not parsed this while loop"
        );
    }
}
