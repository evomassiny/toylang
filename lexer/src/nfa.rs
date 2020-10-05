use std::error::Error;
//use std::str::FromStr;
use std::collections::HashMap;
use std::fmt;
use crate::rule_lexer::RuleToken;
use crate::rule_parser::ParseError;
use crate::rule_ast::{
    RuleExp,
    RuleAst,
    TargetKind,
};

type StateID = usize;

/// A State can be:
/// * the starting state
/// * a final state
/// * a non-final state
#[derive(PartialEq,Eq,Hash,Debug,Clone)]
pub enum State {
    Start,
    End,
    Named(StateID),
}

impl State {
    /// Return a `Named` state that represents the `item_idx` state of the `Sequence`
    /// expressed by the `exp_idx` element of an AST (eg: a [RuleExp]).
    fn from_expression_and_item_indices(exp_idx: usize, item_idx: usize) -> State {
        return Self::Named(exp_idx + item_idx);
    }
}

/// Represents a transition between 2 states in an Non-Finite Automata
#[derive(PartialEq,Eq,Debug)]
pub struct Transition {
    /// the condition to fullfill in order to follow this transition
    /// `None` represents an Epsilon transistion
    condition: Option<TargetKind>,
    /// Which state this transition points to
    target: State,
}

pub trait NfaBuilder {
    
    /// Returns the index of the nearest parent that is a `Sequence`, and the
    /// position of the closest relative of `n` among its siblings.
    /// Returns None when `n` as no `Sequence` parent.
    fn find_parent_sequence(&self, n: usize) -> Option<(usize, usize)>;
    
    /// Returns the next state (the one from which this expression points toward).
    fn right_state(&self, n: usize) -> Option<State>;
    
    /// Returns the previous state (the one from which this expression starts from)
    fn left_state(&self, n: usize) -> Option<State>;
}

impl NfaBuilder for [RuleExp] {

    fn find_parent_sequence(&self, n: usize) -> Option<(usize, usize)> {
        use RuleExp::*;
        let mut cursor: usize = n;
        // traverse AST hierarchy backward until we reach a `Sequence` expression
        while let Some(idx) = self.parent_index(cursor) {
            match self[idx] {
                Sequence(_item_nb) => {
                    // find the item position
                    let mut item_idx = idx + 1; // index of the first item
                    if item_idx == self.len() {
                        // unreachable unless the AST is incomplete,
                        // because the last elment of self[..] can't be 
                        // a sequence expression NOT followed by its items
                        return None;
                    }
                    // count every item until `cursor` match the item index
                    let mut item_count: usize = 0;
                    while item_idx != cursor {
                        match self.right_bound(item_idx) {
                            None => break,
                            Some(next_item) => {
                                item_idx = next_item;
                                item_count += 1;
                            },
                        }
                    }
                    return Some((idx, item_count));
                },
                _ => {
                    cursor = idx;
                }
            }
        }
        None
    }

    fn right_state(&self, n: usize) -> Option<State> {
        // This relies on the fact that we can construct a NFA 
        // only using states in-between each items of a Sequence and 
        // 2 other states: end + start
        let mut cursor = n;
        'bottom_up_ast_traversal: loop {
            match self.find_parent_sequence(cursor) {
                None => return Some(State::End),
                Some((sequence_idx, item_pos)) => {
                    match self.get(sequence_idx) {
                        Some(RuleExp::Sequence(item_count)) => {
                            // find out if `self[n]` is not a sub-expression of the 
                            // last item of the sequence.
                            // If so: lookup for the right state of the parent expression
                            if *item_count == item_pos + 1 {
                                cursor = sequence_idx;
                                continue 'bottom_up_ast_traversal;
                            }
                            // otherwise we found the right state, identified 
                            // by the couple (sequence_idx, item_pos)
                            return Some(State::from_expression_and_item_indices(sequence_idx, item_pos));
                        },
                        _ => {
                            // unreachable unless the AST is broken
                            // `find_parent_sequence` must always returns the 
                            // index of a RuleExp::Sequence node
                            return None;
                        }

                    }
                }
            }
        }
    }

    fn left_state(&self, n: usize) -> Option<State> {
        // This relies on the fact that we can construct a NFA 
        // only using states in-between each items of a Sequence and 
        // 2 other states: end + start
        let mut cursor = n;
        'bottom_up_ast_traversal: loop {
            match self.find_parent_sequence(cursor) {
                None => return Some(State::Start),
                Some((sequence_idx, item_pos)) => {
                    match self.get(sequence_idx) {
                        Some(RuleExp::Sequence(_item_count)) => {
                            // find out if `self[n]` is not a sub-expression of the 
                            // first item of the sequence.
                            // If so: lookup for the right state of the parent expression
                            if item_pos == 0 {
                                cursor = sequence_idx;
                                continue 'bottom_up_ast_traversal;
                            }
                            // otherwise we found the left state, identified 
                            // by the couple (sequence_idx, item_pos - 1 )
                            // (we substract one to get the index of the LEFT state)
                            return Some(State::from_expression_and_item_indices(sequence_idx, item_pos - 1));
                        },
                        _ => {
                            // unreachable unless the AST is broken
                            // `find_parent_sequence` must always returns the 
                            // index of a RuleExp::Sequence node
                            return None;
                        }
                    }
                }
            }
        }
    }
}


/// a Non-Finite Automata
#[derive(PartialEq,Eq,Debug)]
pub struct NFA {
    states: HashMap<State, Vec<Transition>>,
}


impl NFA {

    fn from_ast(ast: &[RuleExp]) -> Result<Self, ParseError> {
        use RuleExp::*;
        use State::*;

        // outputs
        let mut states: HashMap<State, Vec<Transition>> = HashMap::new();
        for idx in 0..ast.len() {
            // lookup left and right states 
            let left = ast.left_state(idx)
                .ok_or_else(|| ParseError::new("no previous state"))?;
            let right = ast.right_state(idx)
                .ok_or_else(|| ParseError::new("no next state"))?;

            // build their transitions tables
            let mut left_state_transitions: Vec<Transition> = Vec::new();
            let mut right_state_transitions: Vec<Transition> = Vec::new();

            match ast[idx] {
                Target(ref kind) => { 
                    left_state_transitions.push(
                        Transition { 
                            condition: Some(*kind),
                            target: right.clone(),
                        }
                    );
                },
                Variants(_) | Sequence(_) | Fence => {},
                AtLeastOnce => {
                    // add epsilon transition between
                    // right to left states
                    right_state_transitions.push(
                        Transition { 
                            condition: None,
                            target: left.clone(),
                        }
                    );
                },
                Any => {
                    // add epsilon transition between
                    // right to left states
                    // AND left to right states
                    right_state_transitions.push(
                        Transition { 
                            condition: None,
                            target: left.clone(),
                        }
                    );
                    left_state_transitions.push(
                        Transition { 
                            condition: None,
                            target: right.clone(),
                        }
                    );
                },
                Optional => {
                    // add epsilon transition between
                    // left to right states
                    left_state_transitions.push(
                        Transition { 
                            condition: None,
                            target: right.clone(),
                        }
                    );
                },
            }

            // add transitions to left state
            // (creating one if none exists)
            let transitions: &mut Vec<Transition> = states.entry(left)
                .or_insert_with(|| Vec::new());
            if !left_state_transitions.is_empty() {
                transitions.append(&mut left_state_transitions);
            }
            // add transitions to right state
            // (creating one if none exists)
            let transitions: &mut Vec<Transition> = states.entry(right)
                .or_insert_with(|| Vec::new());
            if !right_state_transitions.is_empty() {
                transitions.append(&mut right_state_transitions);
            }
        }
        Ok(NFA { states })
    }

}

#[test]
fn find_parent_sequence() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "a(b|c?)"
    let ast = vec![
        Sequence(2),
        Target(Literal('a')),
        Variants(2),
        Target(Literal('b')), 
        Optional,
        Target(Literal('c')), 
    ];

    assert_eq!(
        ast.find_parent_sequence(0),
        None,
    );
    assert_eq!(
        ast.find_parent_sequence(5),
        Some((0, 1)), // 0 => ast[0], 1 => items[1] of the sequences ast[0]
    );
    assert_eq!(
        ast.find_parent_sequence(1),
        Some((0, 0)),
    );
    
    // ast for "a+(bcd|d)?"
    let ast = vec![
        Sequence(2),
        AtLeastOnce,
        Target(Literal('a')),
        Optional,
        Variants(2),
        Sequence(3),
        Target(Literal('b')), 
        Target(Literal('c')), 
        Target(Literal('d')), 
        Target(Literal('e')), 
    ];
    assert_eq!(
        ast.find_parent_sequence(9),
        Some((0, 1)),
    );
}

#[test]
fn left_state_lookup() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "a+(bcd|d)?"
    let ast = vec![
        Sequence(2),
        AtLeastOnce,
        Target(Literal('a')),
        Optional,
        Variants(2),
        Sequence(3),
        Target(Literal('b')), 
        Target(Literal('c')), 
        Target(Literal('d')), 
        Target(Literal('e')), 
    ];

    assert_eq!(
        ast.left_state(0),
        Some(State::Start),
    );
    assert_eq!(
        ast.left_state(1),
        Some(State::Start),
    );
    assert_eq!(
        ast.left_state(2),
        Some(State::Start),
    );

    assert_eq!(
        ast.left_state(7),
        Some(State::from_expression_and_item_indices(5, 0)),
    );

    assert_eq!(
        ast.left_state(9),
        Some(State::from_expression_and_item_indices(0, 0)),
    );
    
    // ast for "abc"
    let ast = vec![
        Sequence(3),
        Target(Literal('a')), 
        Target(Literal('b')), 
        Target(Literal('c')), 
    ];
    assert_eq!(
        ast.left_state(1),
        Some(State::Start),
    );
    assert_eq!(
        ast.left_state(2),
        Some(State::from_expression_and_item_indices(0, 0)),
    );
    assert_eq!(
        ast.left_state(3),
        Some(State::from_expression_and_item_indices(0, 1)),
    );
}

#[test]
fn right_state_lookup() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "abc"
    let ast = vec![
        Sequence(3),
        Target(Literal('a')), 
        Target(Literal('b')), 
        Target(Literal('c')), 
    ];
    assert_eq!(
        ast.right_state(1),
        Some(State::from_expression_and_item_indices(0, 0)),
    );
    assert_eq!(
        ast.right_state(2),
        Some(State::from_expression_and_item_indices(0, 1)),
    );
    assert_eq!(
        ast.right_state(3),
        Some(State::End),
    );

    // ast for "a+(bcd|d)?"
    let ast = vec![
        Sequence(2),
        AtLeastOnce,
        Target(Literal('a')),
        Optional,
        Variants(2),
        Sequence(3),
        Target(Literal('b')), 
        Target(Literal('c')), 
        Target(Literal('d')), 
        Target(Literal('e')), 
    ];

    assert_eq!(
        ast.right_state(0),
        Some(State::End),
    );
    assert_eq!(
        ast.right_state(1),
        Some(State::from_expression_and_item_indices(0, 0)),
    );
    assert_eq!(
        ast.right_state(2),
        Some(State::from_expression_and_item_indices(0, 0)),
    );

    assert_eq!(
        ast.right_state(7),
        Some(State::from_expression_and_item_indices(5, 1)),
    );

    assert_eq!(
        ast.right_state(9),
        Some(State::End),
    );
}

#[test]
fn nfa_target() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "a"
    let ast = vec![
        Target(Literal('a')),
    ];

    let mut states = HashMap::new();
    states.insert(
        State::Start,
        vec![
            Transition { condition: Some(Literal('a')), target: State::End }, 
        ]
    );
    states.insert(
        State::End,
        vec![]
    );
    assert_eq!(
        NFA::from_ast(&ast),
        Ok(NFA { states: states }),
    );
}

#[test]
fn nfa_sequence() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "abc"
    let ast = vec![
        Sequence(3),
        Target(Literal('a')),
        Target(Literal('b')),
        Target(Literal('c')),
    ];

    let mut states = HashMap::new();
    states.insert(
        State::Start,
        vec![
            Transition { condition: Some(Literal('a')), target: State::Named(0) }, 
        ]
    );
    states.insert(
        State::Named(0),
        vec![
            Transition { condition: Some(Literal('b')), target: State::Named(1) }, 
        ]
    );
    states.insert(
        State::Named(1),
        vec![
            Transition { condition: Some(Literal('c')), target: State::End }, 
        ]
    );
    states.insert(
        State::End,
        vec![]
    );
    assert_eq!(
        NFA::from_ast(&ast),
        Ok(NFA { states: states }),
    );
}

#[test]
fn nfa_generation() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "a(b|c?)"
    let ast = vec![
        Sequence(2),
        Target(Literal('a')),
        Variants(2),
        Target(Literal('b')), 
        Optional,
        Target(Literal('c')), 
    ];

    let mut states = HashMap::new();
    states.insert(
        State::Start,
        vec![
            Transition { condition: Some(Literal('a')), target: State::Named(0) }, 
        ]
    );
    states.insert(
        State::Named(0),
        vec![
            Transition { condition: Some(Literal('b')), target: State::End }, 
            Transition { condition: None, target: State::End }, 
            Transition { condition: Some(Literal('c')), target: State::End }, 
        ]
    );
    states.insert(
        State::End,
        vec![]
    );
    assert_eq!(
        NFA::from_ast(&ast),
        Ok(NFA { states: states }),
    );
}
