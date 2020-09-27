use std::error::Error;
//use std::str::FromStr;
use std::collections::HashMap;
use std::fmt;
use crate::rule_lexer::RuleToken;
use crate::rule_parser::{
    TargetKind,
    RuleExp,
    RuleAst,
    ParseError,
};

type StateID = usize;


/// Represents a transition between 2 states in an Non-Finite Automata
pub struct Transition {
    /// the conditrion to fullfill in order to follow this transition
    /// `None` represents an Epsilon transistion
    condition: Option<TargetKind>,
    /// Which state this transition points to
    target: StateID,
}

/// A State can be:
/// * the starting state
/// * a final state
/// * a non-final state
pub enum StateKind {
    Start,
    Final,
    NonFinal,
}

/// Represents a state in a Non-Finite Automata
pub struct State {
    id: StateID,
    kind: StateKind,
}

/// a Non-Finite Automata
pub struct NFA {
    /// each state is located at its index
    states: Vec<State>,
    /// Possible transitions from each state
    /// (same index as `self.states`)
    transitions: Vec<Vec<Transition>>,
    start: StateID,
}

impl NFA {

    fn from_rule_ast(ast: &RuleAst) -> Result<Self, ParseError> {
        use RuleExp::*;
        use StateKind::*;

        // outputs
        let mut states: Vec<State> = vec![
            State { id: 0, kind: Start },
        ];
        let mut transitions: Vec<Vec<Transition>> = vec![vec![]];

        // helps keeping track of the AST traversal
        let mut current_rules: Vec<Vec<&RuleExp>> = vec![vec![&ast.rules]];
        let mut current_rule_ids: Vec<usize> = vec![0];

        // updated at each step of the AST traversal
        let mut rule: &RuleExp;
        let mut depth_idx: usize;
        let mut rule_idx_in_level: usize;

        // traverse the AST
        while current_rules.len() > 0 {

            depth_idx = current_rules.len() - 1;
            rule_idx_in_level = current_rule_ids[depth_idx];
            rule = current_rules[depth_idx][rule_idx_in_level];

            match *rule {
                Once(ref exp) => { ; },
                AtLeastOnce(ref exp) => { ; },
                Any(ref exp) => { ; },
                Optional(ref exp) => { ; },
                Variants(ref exps) => { ; },
                Target(ref kind) => { ; },
                Sequence(ref exps) => { ; },
            }
        }

        Ok(Self { 
            states: states,
            transitions: transitions,
            start: 0usize,
        })
    }

}
