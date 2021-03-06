use crate::ast::parser::{
    FlatExp,
    FlatExp::*,
    Literal,
    UnaryOp,
    BinaryOp::*,
    ComparisonOp,
    LogicalOp,
    NumericalOp,
};
use crate::ast::tokens::{
    TokenKind::*,
    Token,
    LiteralKind::*,
    SeparatorKind::*,
    OperatorKind,
    OperatorKind::*,
    KeywordKind::*,
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

/// match a statement (an expression ending by `;`), only consumes the `;`
/// does match semi columns in a deeper block that the one `tokens[0]` is.
pub fn match_semi_colomn_fenced(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    let mut block_count = 0;
    for i in 0..tokens.len() {
        match tokens[i] {
            Some(&Token { kind: Separator(OpenBlock), ..}) => block_count += 1,
            Some(&Token { kind: Separator(CloseBlock), ..}) => block_count -= 1,
            Some(&Token { kind: Separator(Semicolon), ..}) => {
                if block_count == 0 { 
                    return Some((FlatFenced, vec![i])); 
                }
            },
            None => return None,
            _ => continue,
        }
    }
    None
}

/// match a statement inside parenthesis, consumes the parenthesis
pub fn match_parenthesis_fenced(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    let mut parent_count = 0;
    let mut consumed_tokens =  vec![];
    match tokens[0] {
        Some(&Token { kind: Separator(OpenParen), ..}) => {
            consumed_tokens.push(0);
            parent_count += 1;
        }
        _ => return None,
    }
    for i in 1..tokens.len() {
        match tokens[i] {
            Some(&Token { kind: Separator(OpenParen), ..}) => {
                if parent_count == 0 { consumed_tokens.push(i); }
                parent_count += 1;
            }
            Some(&Token { kind: Separator(CloseParen), ..}) => {
                parent_count -= 1;
                if parent_count == 0 { 
                    consumed_tokens.push(i);
                    return Some((FlatFenced, consumed_tokens)); 
                }
            },
            None => return None,
            _ => continue,
        }
    }
    None
}

/// Match at any place in the current expression:
/// * `id = expr` 
pub fn match_assign(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 3 { return None; }
    let name: String;
    match tokens[0] {
        Some(&Token { kind: Identifier(ref id), ..}) => name = id.into(),
        _ => return None,
    }
    match tokens[1] {
        Some(Token { kind: Operator(OperatorKind::Assign), .. }) => {
            Some((FlatExp::FlatAssign(name) , vec![0, 1]))
        },
        _ => None,
    }
}

/// Match at any place in the current expression:
/// * `expr == expr` 
/// * `expr != expr` 
/// * `expr >= expr` 
/// * `expr <= expr` 
/// * `expr > expr` 
/// * `expr < expr` 
/// * `expr + expr` 
/// * `expr - expr` 
/// * `expr & expr` 
/// * `expr | expr` 
/// * `expr * expr` 
/// * `expr / expr` 
/// * `expr ** expr` 
/// * `expr % expr` 
/// (in this order of priority)
/// Unless they are in between paranthesis
pub fn match_binary_op(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 3 { return None; }
    // the operator token must be at least the index 1
    // which ensure that we don't match unary operation
    const START_IDX: usize = 1; 

    // parse comparison operations
    let mut paren_count = 0;
    for i in 0..tokens.len() {
        match tokens[i] {
            Some(Token { kind: Separator(OpenParen), .. }) => paren_count += 1,
            Some(Token { kind: Separator(CloseParen), .. }) => paren_count -= 1,
            Some(Token { kind: Operator(operator), .. }) => {
                if paren_count == 0 && i >= START_IDX {
                    match operator {
                        Equal => return Some((FlatBinaryOp(Comparison(ComparisonOp::Equal)), vec![i])),
                        NotEq => return Some((FlatBinaryOp(Comparison(ComparisonOp::NotEqual)), vec![i])),
                        GreaterThan => return Some((FlatBinaryOp(Comparison(ComparisonOp::GreaterThan)), vec![i])),
                        GreaterThanOrEq => return Some((FlatBinaryOp(Comparison(ComparisonOp::GreaterThanOrEqual)), vec![i])),
                        LessThan => return Some((FlatBinaryOp(Comparison(ComparisonOp::LessThan)), vec![i])),
                        LessThanOrEq => return Some((FlatBinaryOp(Comparison(ComparisonOp::LessThanOrEqual)), vec![i])),
                        _ => continue,
                    }
                }
            },
            None => break,
            _ => continue,
        }
    }
    // parse low priority arithmetic operation (+, -)
    paren_count = 0;
    for i in 0..tokens.len() {
        match tokens[i] {
            Some(Token { kind: Separator(OpenParen), .. }) => paren_count += 1,
            Some(Token { kind: Separator(CloseParen), .. }) => paren_count -= 1,
            Some(Token { kind: Operator(operator), .. }) => {
                if paren_count == 0 && i >= START_IDX {
                    match operator {
                        Add => return Some((FlatBinaryOp(Numerical(NumericalOp::Add)), vec![i])),
                        Sub => return Some((FlatBinaryOp(Numerical(NumericalOp::Sub)), vec![i])),
                        _ => continue,
                    }
                }
            },
            None => break,
            _ => continue,
        }
    }
    // parse logical operation (||, &&)
    paren_count = 0;
    for i in 0..tokens.len() {
        match tokens[i] {
            Some(Token { kind: Separator(OpenParen), .. }) => paren_count += 1,
            Some(Token { kind: Separator(CloseParen), .. }) => paren_count -= 1,
            Some(Token { kind: Operator(operator), .. }) => {
                if paren_count == 0 && i >= START_IDX {
                    match operator {
                        And => return Some((FlatBinaryOp(Logical(LogicalOp::And)), vec![i])),
                        Or => return Some((FlatBinaryOp(Logical(LogicalOp::Or)), vec![i])),
                        _ => continue,
                    }
                }
            },
            None => break,
            _ => continue,
        }
    }
    // parse medium priority arithmetic operation (*, /, %)
    paren_count = 0;
    for i in 0..tokens.len() {
        match tokens[i] {
            Some(Token { kind: Separator(OpenParen), .. }) => paren_count += 1,
            Some(Token { kind: Separator(CloseParen), .. }) => paren_count -= 1,
            Some(Token { kind: Operator(operator), .. }) => {
                if paren_count == 0 && i >= START_IDX {
                    match operator {
                        Mul => return Some((FlatBinaryOp(Numerical(NumericalOp::Mul)), vec![i])),
                        Div => return Some((FlatBinaryOp(Numerical(NumericalOp::Div)), vec![i])),
                        Mod => return Some((FlatBinaryOp(Numerical(NumericalOp::Mod)), vec![i])),
                        _ => continue,
                    }
                }
            },
            None => break,
            _ => continue,
        }
    }
    // parse high priority arithmetic operation (**)
    paren_count = 0;
    for i in 0..tokens.len() {
        match tokens[i] {
            Some(Token { kind: Separator(OpenParen), .. }) => paren_count += 1,
            Some(Token { kind: Separator(CloseParen), .. }) => paren_count -= 1,
            Some(Token { kind: Operator(operator), .. }) => {
                if paren_count == 0 && i >= START_IDX {
                    match operator {
                        Pow => return Some((FlatBinaryOp(Numerical(NumericalOp::Pow)), vec![i])),
                        _ => continue,
                    }
                }
            },
            None => break,
            _ => continue,
        }
    }
    None
}

/// Match from the first item of `token`:
/// * `-` followed by any token 
/// * `+` followed by any token 
/// * `!` followed by any token 
/// * `--` followed by any token 
/// * `++` followed by any token 
/// * any token followed by `--`
/// * any token followed by `++`
pub fn match_unary_op(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 2 || tokens[1].is_none() {
        return None; 
    }
    return match (tokens[0], tokens[1]) {
        // match '+foo', '-foo', '!foo' 
        (Some(&Token { kind: Operator(Add), ..}), _) => Some((FlatUnaryOp(UnaryOp::Plus), vec![0])),
        (Some(&Token { kind: Operator(Sub), ..}), _) => Some((FlatUnaryOp(UnaryOp::Minus), vec![0])),
        (Some(&Token { kind: Operator(Not), ..}), _) => Some((FlatUnaryOp(UnaryOp::Not), vec![0])),
        // match '++foo' and '--foo'
        (Some(&Token { kind: Operator(Inc), .. }), Some(&Token { kind: Identifier(..), ..})) => Some((FlatUnaryOp(UnaryOp::PreInc), vec![0])),
        (Some(&Token { kind: Operator(Dec), .. }), Some(&Token { kind: Identifier(..), ..})) => Some((FlatUnaryOp(UnaryOp::PreDec), vec![0])),
        // match 'foo++' and 'foo--'
        (Some(&Token { kind: Identifier(..), ..}), Some(&Token { kind: Operator(Inc), .. })) => Some((FlatUnaryOp(UnaryOp::PostInc), vec![1])),
        (Some(&Token { kind: Identifier(..), ..}), Some(&Token { kind: Operator(Dec), .. })) => Some((FlatUnaryOp(UnaryOp::PostDec), vec![1])),
        _ => None,
    };
}

/// Match a number, a quoted string or a boolean litteral at the first itme of `tokens`
/// Match from the first item of `token`:
/// * A string litteral
/// * A number 
/// * A boolean litteral
/// * `null`
/// * `undefined`
pub fn match_const(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.is_empty() { return None; }
    match tokens[0] {
        Some(Token { kind: Literal(literal), ..} ) => {
            match literal {
                Boolean(value) => return Some((FlatLiteral(Literal::Bool(*value)),vec![0])),
                Numeric(value) => return Some((FlatLiteral(Literal::Num(*value)), vec![0])),
                Str(value) => return Some((FlatLiteral(Literal::Str(value.into())), vec![0])),
            }
        },
        Some(Token { kind: Keyword(keyword), ..} ) => {
            match keyword {
                Null => return Some((FlatLiteral(Literal::Null), vec![0])),
                Undefined => return Some((FlatLiteral(Literal::Undefined), vec![0])),
                _ => {}
            }
        },
        _ => {}
    }
    None
}

/// Match Jumpers expressions
/// * `break`
/// * `continue`
pub fn match_jumpers(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.is_empty() { return None; }
    match tokens[0] {
        Some(Token { kind: Keyword(keyword), ..} ) => {
            match keyword {
                Break => return Some((FlatBreak, vec![0])),
                Continue => return Some((FlatContinue, vec![0])),
                _ => {}
            }
        },
        _ => {}
    }
    None
}

/// Match a local label, eg: a variable name from the first item of `tokens':
pub fn match_local(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.is_empty() { return None; }
    if let Some(Token { kind: Identifier(ref id), ..} ) = tokens[0] {
        return Some((FlatLocal(id.into()), vec![0]));
    }
    None
}

/// match a block starting at the 1st item of `tokens`
pub fn match_block(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 2 { return None; }
    let mut consumed_tokens =  vec![];
    // match `{`
    match tokens[0] {
        Some(&Token { kind: Separator(OpenBlock), ..}) => consumed_tokens.push(0),
        _ => return None,
    }
    // iter until match `}`
    let mut i = 1;
    let mut block_count = 1;
    let mut sub_expr_count = 0;
    while i  < tokens.len() {
        match tokens[i] {
            None => return None,
            Some(token) => {
                match *token {
                    Token { kind: Separator(OpenBlock), ..} => block_count += 1,
                    // count the number of sub-expressions
                    Token { kind: Separator(Semicolon), ..} 
                        | Token { kind: Keyword(If), ..}
                        | Token { kind: Keyword(While), ..}
                        | Token { kind: Keyword(Function), ..}
                        => {
                        if block_count == 1 {
                            sub_expr_count += 1; 
                        }
                    },
                    Token { kind: Separator(CloseBlock), ..} => {
                        block_count -= 1;
                        //  If we reached the end of the block
                        if block_count == 0 {
                            // if we skip several token it must 
                            // be at least one sub-expression (eg: {{stuff;}})
                            if sub_expr_count == 0 && i > 2 {
                                sub_expr_count += 1;
                            }
                            consumed_tokens.push(i);
                            return Some((FlatBlock(sub_expr_count), consumed_tokens));
                        }
                    },
                    _ => {}
                }
            }
        }
        i += 1;
    }
    None
}

/// match a function call starting at the 1st item of `tokens`
pub fn match_function_call(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 3 { return None; }
    let mut consumed_tokens = vec![];
    let mut sub_expr_count = 0;
    //let name: String;
    // match identifier
    match tokens[0] {
        Some(&Token { kind: Identifier(_), ..}) => {
            //consumed_tokens.push(0);
            //name = id.into();
            sub_expr_count += 1;
        },
        _ => return None,
    }
    // TODO: here we should only match the rightmost parenthesis group
    // match `(`
    match tokens[1] {
        Some(&Token { kind: Separator(OpenParen), ..}) => consumed_tokens.push(1),
        _ => return None,
    }
    // iter until match `)`
    let mut paren_count = 1;
    let mut i = 2;
    while i < tokens.len() {
        match tokens[i] {
            Some(&Token { kind: Separator(OpenParen), ..}) => paren_count += 1,
            Some(&Token { kind: Separator(CloseParen), ..}) => {
                paren_count -= 1;
                if paren_count == 0 {
                    consumed_tokens.push(i);
                    // if the close parenthesis was not direcly after the open 
                    // parenthesis, assume at least one expression exists
                    if i > 2 { sub_expr_count += 1; }
                    return Some((FlatCall(sub_expr_count), consumed_tokens));
                }
            },
            Some(&Token { kind: Separator(Comma), ..}) => {
                consumed_tokens.push(i);
                sub_expr_count +=1;
            },
            None => return None,
            _ => {},
        }
        i += 1;
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
/// * `for (expression; expression; expression) {block}`
pub fn match_for_loop(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 4 { return None; }
    let mut consumed_tokens = vec![];
    // match `while`
    match tokens[0] {
        Some(&Token { kind: Keyword(For), ..}) => consumed_tokens.push(0),
        _ => return None,
    }
    // match `(`
    match tokens[1] {
        Some(&Token { kind: Separator(OpenParen), ..}) => consumed_tokens.push(1),
        _ => return None,
    }
    // iter until match `;`
    let mut i = 2;
    while i < tokens.len() {
        match tokens[i] {
            Some(&Token { kind: Separator(Semicolon), ..}) => {
                consumed_tokens.push(i);
                break;
            },
            _ => i += 1,
        }
    }
    // iter until match `;`
    i += 1;
    while i < tokens.len() {
        match tokens[i] {
            Some(&Token { kind: Separator(Semicolon), ..}) => {
                consumed_tokens.push(i);
                break;
            },
            _ => i += 1,
        }
    }

    // iter until match `)`
    i += 1;
    let mut paren_count = 1;
    while i < tokens.len() {
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
            _ => {},
        }
        i += 1;
    }
    // match {group}
    i += 1; 
    if i >= tokens.len() {
        return None;
    }
    let _ = consume_block(&tokens[i..])?; // go at CloseBlock
    Some((FlatForLoop, consumed_tokens))
}
/// match (starting from the 1st token):
/// `fn identifier( {block}`
pub fn match_function_declaration(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
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
    if tokens.is_empty() { return None; }
    if let Some(&Token { kind: Keyword(Return), ..}) = tokens[0] {
        let sub_expr_count = if tokens.len() > 1 && !tokens[1].is_none() { 1 } else { 0 };
        return Some((FlatReturn(sub_expr_count), vec![0]));
    }
    None
}

/// match (starting from the 1st token):
/// * `let identifier;`
/// * `let identifier = expression`
pub fn match_let(tokens: &[Option<&Token>]) -> Option<(FlatExp, Vec<usize>)> {
    if tokens.len() < 3 { return None; }
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
    // match `=` or None
    match tokens[2] {
        Some(&Token { kind: Operator(OperatorKind::Assign), ..}) => {
            // imcomplete statement like `let a = `
            if tokens.len() < 4 {
                return None;
            }
        },
        // case `let id;` 
        None => return Some((FlatLetDecl(name, 0), vec![0, 1])),
        _ => return None,
    }
    // case `let id = expression;` 
    Some((FlatLetDecl(name, 1), vec![0, 1, 2]))
}

#[cfg(test)]
mod test {
    use crate::ast::patterns;
    use crate::ast::tokens::Token;
    use crate::ast::lexer::lex;
    use crate::ast::parser::{
        FlatExp,
        Literal,
        UnaryOp,
        BinaryOp::*,
        NumericalOp,
        LogicalOp,
    };

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
        // test sub expression count: case `if`
        let tokens = lex("{if (true) {} else {}}").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_block(&unparsed_tokens),
            Some((FlatExp::FlatBlock(1), vec![0_usize, 10])),
            "Failed to match block pattern"
        );
        // test sub expression count: case `while`
        let tokens = lex("{ while (true) {} }").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_block(&unparsed_tokens),
            Some((FlatExp::FlatBlock(1), vec![0_usize, 7])),
            "Failed to match block pattern"
        );
        // test sub expression count: case `function`
        let tokens = lex("{ function a() {} }").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_block(&unparsed_tokens),
            Some((FlatExp::FlatBlock(1), vec![0_usize, 7])),
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
            patterns::match_function_call(&unparsed_tokens),
            Some((FlatExp::FlatCall(1), vec![1, 2])),
            "Failed to match function call pattern"
        );
        // test call with arguments
        let tokens = lex("call(1,(1+3))").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_function_call(&unparsed_tokens),
            Some((FlatExp::FlatCall(3), vec![1, 3, 9])),
            "Failed to match function call pattern"
        );
        // test invalid
        let tokens = lex("call(1,").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_function_call(&unparsed_tokens),
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
            "Should have not parsed this while loop"
        );
        // test invalid while
        let tokens = lex("while ( a ").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_while(&unparsed_tokens),
            None,
            "Should have not parsed this while loop"
        );
    }

    #[test]
    fn match_for_loop_pattern() {
        // test simple while
        let tokens = lex(r#"
        for ( i = 0; i < 10; i++ ) {
            a = a + i;
        }
        "#).unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_for_loop(&unparsed_tokens),
            Some((FlatExp::FlatForLoop, vec![0_usize, 1, 5, 9, 12])),
            "Failed to match for pattern"
        );
    }

    #[test]
    fn match_let_pattern() {
        // test variable declaration
        let tokens = lex("let a = 1;").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_let(&unparsed_tokens),
            Some((FlatExp::FlatLetDecl("a".into(), 1), vec![0_usize, 1, 2])),
            "Failed to match let pattern"
        );
        // test variable declaration without value assignment
        let tokens = lex("let a").unwrap();
        let mut unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        unparsed_tokens.push(None); // simulate semicolon
        assert_eq!(
            patterns::match_let(&unparsed_tokens),
            Some((FlatExp::FlatLetDecl("a".into(), 0), vec![0_usize, 1])),
            "Failed to match let pattern without assignment"
        );
        // test variable declaration
        let tokens = lex("let a = ").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_let(&unparsed_tokens),
            None,
            "Matched imcomplete statements"
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
        function foo(a, b) {
            return a + b;
        }
        "#).unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_function_declaration(&unparsed_tokens),
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

    #[test]
    fn match_semi_colomn_fenced_pattern() {
        let tokens = lex("let a= 1; a = 2 * a;").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_semi_colomn_fenced(&unparsed_tokens),
            Some((
                FlatExp::FlatFenced,
                vec![4]
            )),
            "Failed to match line statement"
        );
        // don't match statement in subexpressions
        let tokens = lex("{ let a = 1; a = 2 * a;}").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_semi_colomn_fenced(&unparsed_tokens),
            None,
            "should ignore semi columns in subexpressions "
        );
        // nothing to parse
        let tokens = lex("let a = 1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_semi_colomn_fenced(&unparsed_tokens),
            None,
            "Failed to match line statement"
        );
    }

    #[test]
    fn match_parenthesis_fenced_pattern() {
        let tokens = lex("(1 +1) * 2;").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_parenthesis_fenced(&unparsed_tokens),
            Some((
                FlatExp::FlatFenced,
                vec![0, 4]
            )),
            "Failed to match parenthesis statement"
        );
        // don't match statement in subexpressions
        let tokens = lex("( let a = 1;").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_parenthesis_fenced(&unparsed_tokens),
            None,
            "should ignore unclosed parenthesis "
        );
    }

    #[test]
    fn match_const_pattern() {
        let tokens = lex("1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_const(&unparsed_tokens),
            Some((
                FlatExp::FlatLiteral(Literal::Num(1.)),
                vec![0]
            )),
            "Failed to match const literal"
        );
        let tokens = lex("true").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_const(&unparsed_tokens),
            Some((
                FlatExp::FlatLiteral(Literal::Bool(true)),
                vec![0]
            )),
            "Failed to match const literal"
        );
        let tokens = lex(r#""bim""#).unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_const(&unparsed_tokens),
            Some((
                FlatExp::FlatLiteral(Literal::Str("bim".into())),
                vec![0]
            )),
            "Failed to match const literal"
        );
        let tokens = lex("null").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_const(&unparsed_tokens),
            Some((
                FlatExp::FlatLiteral(Literal::Null),
                vec![0]
            )),
            "Failed to match const literal null"
        );
        let tokens = lex("undefined").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_const(&unparsed_tokens),
            Some((
                FlatExp::FlatLiteral(Literal::Undefined),
                vec![0]
            )),
            "Failed to match const literal undefined"
        );
    }

    #[test]
    fn match_unary_op_pattern() {
        // test minus
        let tokens = lex("-1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_unary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatUnaryOp(UnaryOp::Minus),
                vec![0]
            )),
            "Failed to match unary operation"
        );
        // test plus
        let tokens = lex("+1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_unary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatUnaryOp(UnaryOp::Plus),
                vec![0]
            )),
            "Failed to match unary operation"
        );
        // test not
        let tokens = lex("!foo").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_unary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatUnaryOp(UnaryOp::Not),
                vec![0]
            )),
            "Failed to match unary operation"
        );
        // test post increment
        let tokens = lex("foo++").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_unary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatUnaryOp(UnaryOp::PostInc),
                vec![1]
            )),
            "Failed to match unary operation ++"
        );
        // test post decrement
        let tokens = lex("foo--").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_unary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatUnaryOp(UnaryOp::PostDec),
                vec![1]
            )),
            "Failed to match unary operation --"
        );
        // test pre increment
        let tokens = lex("++foo").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_unary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatUnaryOp(UnaryOp::PreInc),
                vec![0]
            )),
            "Failed to match unary operation --"
        );
        // test pre decrement
        let tokens = lex("--foo").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_unary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatUnaryOp(UnaryOp::PreDec),
                vec![0]
            )),
            "Failed to match unary operation --"
        );
    }

    #[test]
    fn match_binary_op_pattern() {
        let tokens = lex("1-1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_binary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatBinaryOp(Numerical(NumericalOp::Sub)),
                vec![1]
            )),
            "Failed to match binary operation"
        );

        let tokens = lex("a + a").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_binary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatBinaryOp(Numerical(NumericalOp::Add)),
                vec![1]
            )),
            "Failed to match binary operation"
        );

        let tokens = lex("f() + f()").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_binary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatBinaryOp(Numerical(NumericalOp::Add)),
                vec![3]
            )),
            "Failed to match binary operation"
        );

        let tokens = lex("1-1 * 1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_binary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatBinaryOp(Numerical(NumericalOp::Sub)),
                vec![1]
            )),
            "Failed to match binary operation"
        );

        let tokens = lex("(1-1 * 1) % 1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_binary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatBinaryOp(Numerical(NumericalOp::Mod)),
                vec![7]
            )),
            "Failed to match binary operation"
        );

        let tokens = lex("-1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_binary_op(&unparsed_tokens),
            None,
            "this is not a binary operation"
        );

        let tokens = lex("a || (1-1)").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_binary_op(&unparsed_tokens),
            Some((
                FlatExp::FlatBinaryOp(Logical(LogicalOp::Or)),
                vec![1]
            )),
            "Failed to match binary operation"
        );
    }

    #[test]
    fn match_local_pattern() {
        let tokens = lex("a").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_local(&unparsed_tokens),
            Some((
                FlatExp::FlatLocal("a".into()),
                vec![0]
            )),
            "Failed to match local"
        );
    }

    #[test]
    fn match_assign_pattern() {
        let tokens = lex("a = 1").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_assign(&unparsed_tokens),
            Some((
                FlatExp::FlatAssign("a".into()),
                vec![0, 1]
            )),
            "Failed to match assign"
        );
    }

    #[test]
    fn match_jumpers() {
        let tokens = lex("break").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_jumpers(&unparsed_tokens),
            Some((
                FlatExp::FlatBreak,
                vec![0]
            )),
            "Failed to match break"
        );
        let tokens = lex("continue").unwrap();
        let unparsed_tokens: Vec<Option<&Token>> = tokens.iter().map(|t| Some(t)).collect();
        assert_eq!(
            patterns::match_jumpers(&unparsed_tokens),
            Some((
                FlatExp::FlatContinue,
                vec![0]
            )),
            "Failed to match continue"
        );
    }
}
