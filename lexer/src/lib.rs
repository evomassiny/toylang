pub mod rule_lexer;
pub mod rule_parser;
pub mod rule_ast;
pub mod nfa;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
