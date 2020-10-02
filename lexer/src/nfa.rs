use std::error::Error;
//use std::str::FromStr;
use std::collections::HashMap;
use std::fmt;
use crate::rule_lexer::RuleToken;
use crate::rule_parser::{
    TargetKind,
    FlatRuleExp,
    FlatAst,
    ParseError,
};

type StateID = usize;

/// This struct's only job is to generates uniq 
/// StateID (basically IDs)
#[derive(Debug,Clone,PartialEq)]
pub struct IDGenerator {
    state_id: StateID,
}

impl IDGenerator {
    pub fn new() -> Self {
        Self { state_id: 0 }
    }

    pub fn new_id(&mut self) -> StateID {
        let state_id = self.state_id;
        self.state_id += 1;
        state_id
    }
}

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

    fn from_flat_ast(ast: &[FlatRuleExp]) -> Result<Self, ParseError> {
        use FlatRuleExp::*;
        use StateKind::*;

        // produces uniq states labels
        let mut id_generator = IDGenerator::new();

        // outputs
        let mut states: Vec<State> = vec![
            State { id: id_generator.new_id(), kind: Start },
        ];
        let mut transitions: Vec<Vec<Transition>> = vec![vec![]];

        // main work queue 
        let mut ids_to_process: Vec<Vec<usize>> = vec![(0..ast.len()).rev().collect()];
        // either or not we stop the processing of an expression 
        // to process its the expression children
        let mut in_process: Vec<bool> = ast.iter().map(|_| false).collect();

        'ast_traversal: while !ids_to_process.is_empty() {
            // solve the deepest level first
            let mut depth_idx = ids_to_process.len() - 1;
            'level_traversal: while let Some(idx) = ids_to_process[depth_idx].pop() {
                let node: &FlatRuleExp = &ast[idx];
                if let Some(indices) = ast.sub_exp_indices(idx) {
                    ids_to_process.push(indices);
                    // set the current node as "in process"
                    in_process[idx] = true;
                    continue 'ast_traversal;
                }

                // process the node
                let end_state_id = id_generator.new_id();
                let end_state = State { 
                    id: end_state_id,
                    kind: NonFinal,
                };
                let mut state_transitions = Vec::new();
                match node {
                    FlatTarget(ref kind) => { 
                        let transition = Transition { 
                            condition: Some(*kind),
                            target: end_state_id,
                        };
                        state_transitions.push(transition);
                    },
                    FlatOnce => { ; },
                    FlatAtLeastOnce => { ; },
                    FlatAny => { ; },
                    FlatOptional => { ; },
                    FlatVariants(n) => { ; },
                    FlatSequence(n) => { ; },
                }
                states.push(end_state);
                transitions.push(state_transitions);
                if ids_to_process[depth_idx].is_empty() {
                    let _ = ids_to_process.pop();
                }

            }
        }

        Ok(Self { 
            states: states,
            transitions: transitions,
            start: 0usize,
        })
    }

}
