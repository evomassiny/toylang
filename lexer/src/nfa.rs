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

/// Represents a transition between 2 states in an Non-Finite Automata
pub struct Transition {
    /// the condition to fullfill in order to follow this transition
    /// `None` represents an Epsilon transistion
    condition: Option<TargetKind>,
    /// Which state this transition points to
    target: StateID,
}

/// A State can be:
/// * the starting state
/// * a final state
/// * a non-final state
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


pub trait NfaBuilder {
    
    /// Returns the index of the nearest parent that is a `Sequence`, and the
    /// position of the closest relative of `n` among its siblings.
    /// Returns None when `n` as no `Sequence` parent.
    fn find_parent_sequence(&self, n: usize) -> Option<(usize, usize)>;
    /// Returns the previous state from which this expression
    /// starts to.
    /// This relies on the fact that we can construct a NFA 
    /// only using states in-between each items of a Sequence and 
    /// 2 other states: end + start
    fn right_state(&self, n: usize) -> Option<State>;
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
                    dbg!(item_count, item_idx, cursor);
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


/// a Non-Finite Automata
pub struct NFA {
    states: HashMap<StateID, Vec<Transition>>,
}


impl NFA {

    fn from_flat_ast(ast: &[RuleExp]) -> Result<Self, ParseError> {
        use RuleExp::*;
        use State::*;

        // outputs
        let mut states: HashMap<StateID, Vec<Transition>> = HashMap::new();

        // main work queue 
        let mut ids_to_process: Vec<Vec<usize>> = vec![(0..ast.len()).rev().collect()];
        // either or not we stop the processing of an expression 
        // to process its the expression children
        let mut in_process: Vec<bool> = ast.iter().map(|_| false).collect();

        'ast_traversal: while !ids_to_process.is_empty() {
            // solve the deepest level first
            let mut depth_idx = ids_to_process.len() - 1;
            'level_traversal: while let Some(idx) = ids_to_process[depth_idx].pop() {
                let node: &RuleExp = &ast[idx];
                if let Some(indices) = ast.sub_exp_indices(idx) {
                    ids_to_process.push(indices);
                    // set the current node as "in process"
                    in_process[idx] = true;
                    continue 'ast_traversal;
                }

                match node {
                    Target(ref kind) => { 
                        //let transition = Transition { 
                            //condition: Some(*kind),
                            //target: end_state_id,
                        //};
                        //state_transitions.push(transition);
                    },
                    Once => { ; },
                    AtLeastOnce => { ; },
                    Any => { ; },
                    Optional => { ; },
                    Variants(n) => { ; },
                    Sequence(n) => { ; },
                }
                if ids_to_process[depth_idx].is_empty() {
                    let _ = ids_to_process.pop();
                }

            }
        }

        Ok(Self { states: states })
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
}
