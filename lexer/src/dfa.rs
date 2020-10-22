use crate::nfa::{NfaBuilder, NfaTransition, State, StateID, NFA};
use crate::rule_ast::TargetKind;
use std::collections::HashMap;
use std::fmt;

/// Represents a transition between 2 states in an Deterministic Finite Automata
#[derive(PartialEq, Eq, Debug)]
pub struct Transition {
    /// the condition to fullfill in order to follow this transition
    /// (differs from `NfaTransition` because a DFA as no epsilon transition)
    condition: TargetKind,
    /// Which state this transition points to
    target: State,
}

/// a Deterministic Finite Automata
#[derive(PartialEq, Eq, Debug)]
pub struct DFA {
    states: HashMap<State, Vec<Transition>>,
}


/*
impl DFA {

    pub fn from(nfa: &NFA) -> Self {
        let mut dfa_states = HashMap::new();

        // store for each states, the list of states that can be reached
        // depending of the input
        let mut reachable_states: HashMap<State, HashMap<Option<TargetKind>, Vec<State>>> = HashMap::new();
        for state in nfa.states.keys() {
            for transition in 


        }

        Self { states: dfa_states }
    }
}
*/
