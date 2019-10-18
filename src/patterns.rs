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
        let mut block_count = 1;
        for i in 1..tokens.len() {
            match tokens[i] {
                Some(&Token { kind: Separator(OpenBlock), ..}) => {
                    block_count += 1;
                },
                Some(&Token { kind: Separator(CloseBlock), ..}) => {
                    block_count -= 1;
                    if block_count == 0 {
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
    let mut consumed_tokens =  vec![];
    // match `{`
    match tokens[0] {
        Some(&Token { kind: Separator(OpenBlock), ..}) => consumed_tokens.push(0),
        _ => return None,
    }
    // iter until match `}`
    let mut i = 0;
    let mut block_count = 1;
    let mut sub_expr_count = 0;
    while (i + 1) < tokens.len() {
        i += 1;
        match tokens[i] {
            Some(&Token { kind: Separator(OpenBlock), ..}) => {
                // only count the direct sub-level blocks as subexpressions
                if block_count == 1 { sub_expr_count += 1; }
                block_count += 1;
            },
            Some(&Token { kind: Separator(Semicolon), ..}) => {
                if block_count == 1 { sub_expr_count += 1; }
            },
            Some(&Token { kind: Separator(CloseBlock), ..}) => {
                block_count -= 1;
                if block_count == 0 {
                    consumed_tokens.push(i);
                    return Some((FlatBlock(sub_expr_count), consumed_tokens));
                }
            },
            None => return None,
            _ => continue,
        }
    }
    None
}

/// match a function call starting at the 1st item of `token`
pub fn match_call(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 3 { return None; }
    let mut consumed_tokens = vec![];
    let name: String;
    // match identifier
    match tokens[0] {
        Some(&Token { kind: Identifier(ref id), ..}) => {
            consumed_tokens.push(0);
            name = id.into();
        },
        _ => return None,
    }
    // match `(`
    match tokens[1] {
        Some(&Token { kind: Separator(OpenParen), ..}) => consumed_tokens.push(1),
        _ => return None,
    }
    // iter until match `)`
    let mut i = 1;
    let mut paren_count = 1;
    let mut sub_expr_count = 0;
    while (i + 1) < tokens.len() {
        i += 1;
        match tokens[i] {
            Some(&Token { kind: Separator(OpenParen), ..}) => paren_count += 1,
            Some(&Token { kind: Separator(CloseParen), ..}) => {
                paren_count -= 1;
                if paren_count == 0 {
                    consumed_tokens.push(i);
                    // one comma means 2 arguments
                    if sub_expr_count > 0 { sub_expr_count += 1; }
                    return Some((FlatCall(name, sub_expr_count), consumed_tokens));
                }
            },
            Some(&Token { kind: Separator(Comma), ..}) => {
                consumed_tokens.push(i);
                sub_expr_count +=1;
            },
            None => return None,
            _ => continue,
        }
    }
    None
}
/// match (starting from the 1st token):
/// * if (expression) {block}
/// * if (expression) {block} else {block}
pub fn match_if(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 4 { return None; }
    let mut consumed_tokens = vec![];
    // match `while`
    match tokens[0] {
        Some(&Token { kind: Keyword(If), ..}) => consumed_tokens.push(0),
        _ => return None,
    }
    // match `(`
    match tokens[1] {
        Some(&Token { kind: Separator(OpenParen), ..}) => consumed_tokens.push(1),
        _ => return None,
    }
    // iter until match `)`
    let mut i = 1;
    let mut paren_count = 1;
    while (i + 1) < tokens.len() {
        i += 1;
        match tokens[i] {
            Some(&Token { kind: Separator(CloseParen), ..}) => {
                paren_count -= 1;
                if paren_count == 0 {
                    consumed_tokens.push(i);
                    break;
                }
            },
            Some(&Token { kind: Separator(OpenParen), ..}) => paren_count += 1,
            None => return None,
            _ => continue,
        }
    }
    // match {group}
    i += 1; 
    if i >= tokens.len() { return None; }
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
    Some((FlatIf(2), consumed_tokens))
}

/// match (starting from the 1st token):
/// * `while (expression) {block}`
pub fn match_while(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 4 { return None; }
    let mut consumed_tokens = vec![];
    // match `while`
    match tokens[0] {
        Some(&Token { kind: Keyword(While), ..}) => consumed_tokens.push(0),
        _ => return None,
    }
    // match `(`
    match tokens[1] {
        Some(&Token { kind: Separator(OpenParen), ..}) => consumed_tokens.push(1),
        _ => return None,
    }
    // iter until match `)`
    let mut i = 1;
    let mut paren_count = 1;
    while (i + 1) < tokens.len() {
        i += 1;
        match tokens[i] {
            Some(&Token { kind: Separator(CloseParen), ..}) => {
                paren_count -= 1;
                if paren_count == 0 {
                    consumed_tokens.push(i);
                    break;
                }
            },
            Some(&Token { kind: Separator(OpenParen), ..}) => paren_count += 1,
            None => return None,
            _ => continue,
        }
    }
    // match {group}
    i += 1; 
    if i >= tokens.len() {
        return None;
    }
    let _ = consume_block(&tokens[i..])?; // go at CloseBlock
    Some((FlatWhileLoop, consumed_tokens))
}

/// match (starting from the 1st token):
/// `fn identifier( {block}`
pub fn match_function_decl(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 6 { return None; }
    let mut consumed_tokens =  vec![];
    let name: String; 
    // match `fn`
    match tokens[0] {
        Some(&Token { kind: Keyword(Function), ..}) => {
            consumed_tokens.push(0);
        },
        _ => return None
    }
    // match fonction name
    match tokens[1] {
        Some(&Token { kind: Identifier(ref id), ..}) => {
            consumed_tokens.push(1);
            name = id.into();
        },
        _ => return None
    }
    // match `(`
    match tokens[2] {
        Some(&Token { kind: Separator(OpenParen), ..}) => {
            consumed_tokens.push(2);
        },
        _ => return None
    }
    // match args until `)`
    let mut i = 2;
    let mut args: Vec<String> = vec![];
    while (i + 1) < tokens.len() {
        i += 1;
        consumed_tokens.push(i);
        match tokens[i] {
            Some(&Token { kind: Separator(CloseParen), ..}) => break,
            Some(&Token { kind: Separator(Comma), ..}) => continue,
            Some(&Token { kind: Identifier(ref id), ..}) => args.push(id.into()),
            _ => return None,
        }
    }
    // match `{block}`
    i += 1;
    if i >= tokens.len() {
        return None;
    }
    let _ = consume_block(&tokens[i..])?;
    Some((FlatFunctionDecl(name, args), consumed_tokens))
}

/// match (starting from the 1st token):
/// * `return`
/// * `return expression`
pub fn match_return(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 1 { return None; }
    if let Some(&Token { kind: Keyword(Return), ..}) = tokens[0] {
        let sub_expr_count = if tokens.len() > 1 && !tokens[1].is_none() { 1 } else { 0 };
        return Some((FlatReturn(sub_expr_count), vec![0]));
    }
    None
}

/// match (starting from the 1st token):
/// * `let identifier = expression`
pub fn match_let(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 4 { return None; }
    let name: String;
    // match `let`
    match tokens[0] {
        Some(&Token { kind: Keyword(Let), ..}) => {},
        _ => return None,
    }
    // match identifier
    match tokens[1] {
        Some(&Token { kind: Identifier(ref id), ..}) => name = id.into(),
        _ => return None,
    }
    // match `=`
    match tokens[2] {
        Some(&Token { kind: Operator(Assign), ..}) => {},
        _ => return None,
    }
    Some((FlatLetDecl(name), vec![0, 1, 2]))
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
            Some((FlatExp::FlatBlock(1), vec![0_usize, 8])),
            "Failed to match block pattern"
        );
        // test unclosed block
        let tokens = lex("{{ let A = 1; }").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_block(&unparsed_tokens),
            None,
        );
        let tokens = lex("{{ let A = 1; ").unwrap();
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
        // test invalid
        let tokens = lex("call(1,").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_call(&unparsed_tokens),
            None,
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
        let tokens = lex(" if ( true ").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_if(&unparsed_tokens),
            None,
            "Failed to match if pattern"
        );
        let tokens = lex(" if ( true ) { ").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_if(&unparsed_tokens),
            None,
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
        // test invalid while
        let tokens = lex("while ( a ").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_while(&unparsed_tokens),
            None,
            "Should hanve not parsed this while loop"
        );
    }

    #[test]
    fn match_let_pattern() {
        // test variable declaration
        let tokens = lex("let a = 1;").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_let(&unparsed_tokens),
            Some((FlatExp::FlatLetDecl("a".into()), vec![0_usize, 1, 2])),
            "Failed to match let pattern"
        );
        // test variable declaration
        let tokens = lex("let a = ").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_let(&unparsed_tokens),
            None,
            "Failed to match let pattern"
        );
    }

    #[test]
    fn match_return_pattern() {
        // test empty return
        let tokens = lex("return").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_return(&unparsed_tokens),
            Some((FlatExp::FlatReturn(0), vec![0])),
            "Failed to match return pattern"
        );
        // test expression return
        let tokens = lex("return foo").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_return(&unparsed_tokens),
            Some((FlatExp::FlatReturn(1), vec![0])),
            "Failed to match return pattern"
        );
    }

    #[test]
    fn match_function_declaration_pattern() {
        let tokens = lex(r#"
        fn foo(a, b) {
            return a + b;
        }
        "#).unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_function_decl(&unparsed_tokens),
            Some((
                FlatExp::FlatFunctionDecl(
                    "foo".into(),
                    vec!["a".into(), "b".into()]
                ),
                vec![0, 1, 2, 3, 4, 5, 6]
            )),
            "Failed to match function declaration pattern"
        );
    }
}
