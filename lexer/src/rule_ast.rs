
#[derive(Debug,PartialEq,Eq,Clone,Copy)]
pub enum TargetKind {
    // targets
    Start,
    End,
    Literal(char),
}

/// A flat representation of an Abstract syntax Tree
#[derive(Debug,PartialEq,Eq)]
pub enum RuleExp {
    /// consumes 1 expressions
    AtLeastOnce,
    /// consumes 1 expressions
    Any,
    /// consumes 1 expressions
    Optional,
    /// consumes `n` expressions, on per variant
    Variants(usize),
    /// consumes 1 expressions
    Target(TargetKind),
    /// consumes `n` expressions
    Sequence(usize),
    /// consumes 0 expressions, delimits expressions
    Fence,
}

impl RuleExp {

    /// return the number of sub expressions
    /// that a rule is made of.
    pub fn sub_expression_count(&self) -> usize {
        match *self {
            Self::AtLeastOnce => 1,
            Self::Any => 1,
            Self::Optional => 1,
            Self::Variants(n) => n,
            Self::Sequence(n) => n,
            Self::Target(_) => 0,
            Self::Fence => 0,
        }
    }
}

pub trait RuleAst {

    /// Returns the right bound of the expression starting at `self[n]`.
    /// Warning: the bound is exclusive, ie: it point to the `RuleExp`
    /// AFTER the expression (even if none exists).
    fn right_bound(&self, n: usize) -> Option<usize>;
    /// Returns all the children node indices and the children of `self[n]`
    fn sub_exp_indices(&self, n: usize) -> Option<Vec<usize>>;
    /// Returns the index of the parent node
    fn parent_index(&self, n: usize) -> Option<usize>;
    /// Returns all the direct children indices of self[n]
    fn children_indices(&self, n: usize) -> Option<Vec<usize>>;
}

impl RuleAst for [RuleExp] {

    /// Returns the index of the last node of the sub-expressions
    /// composing `self[n]` plus one
    fn right_bound(&self, n: usize) -> Option<usize> {
        if n >= self.len() {
            return None;
        }
        let mut idx = n;
        let mut count = self[n].sub_expression_count();
        while count > 0 {
            idx += 1;
            if idx >= self.len() {
                return None;
            }
            count = count - 1 + self[idx].sub_expression_count();
        }
        Some(idx + 1)
    }

    /// Returns all the children node indices and the children of self[n]
    fn sub_exp_indices(&self, n: usize) -> Option<Vec<usize>> {
        let last_idx: usize = self.right_bound(n)?;
        Some(((n + 1)..last_idx).collect())
    }

    /// Returns the index of the parent node
    fn parent_index(&self, n: usize) -> Option<usize> {
        if n >= self.len() || n == 0 {
            return None;
        }
        let mut cursor = n - 1;
        while cursor >= 0 {
            match self[cursor].sub_expression_count() {
                0 => {},
                _ => {
                    match self.right_bound(cursor) {
                        None => {},
                        Some(idx) if n >= idx => {},
                        Some(_) => {
                            return Some(cursor)
                        },
                    }
                }
            }
            cursor -= 1;
        }
        None
    }

    /// Returns all the direct children indices of self[n]
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
                let bound = self.right_bound(n)?;

                while let Some(idx) = self[0..bound].right_bound(child_idx) {
                    if idx >= bound {
                        break;
                    }
                    child_idx = idx;
                    children_ids.push(idx);
                }
                return Some(children_ids);
            }
        }
    }
}

#[test]
fn get_parent_in_AST() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "a|bc|d"
    let ast = vec![
        Variants(3),
        Target(Literal('a')), 
        Sequence(2),
        Target(Literal('b')), 
        Target(Literal('c')), 
        Target(Literal('d')), 
    ];

    assert_eq!(
        ast.parent_index(1),
        Some(0),
    );
    assert_eq!(
        ast.parent_index(2),
        Some(0),
    );
    assert_eq!(
        ast.parent_index(3),
        Some(2),
    );
    assert_eq!(
        ast.parent_index(4),
        Some(2),
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
        ast.parent_index(9),
        Some(4),
    );

}

#[test]
fn get_children_indices() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "a|bc|d"
    let ast = vec![
        Variants(3),
        Target(Literal('a')), 
        Sequence(2),
        Target(Literal('b')), 
        Target(Literal('c')), 
        Target(Literal('d')), 
    ];

    assert_eq!(
        ast.children_indices(0),
        Some(vec![1, 2, 5]),
    );
    assert_eq!(
        ast.children_indices(2),
        Some(vec![3, 4]),
    );
}

#[test]
fn get_sub_expressions_in_AST() {
    use RuleExp::*;
    use TargetKind::*;

    // ast for "a|bc|d"
    let ast = vec![
        Variants(3),
        Target(Literal('a')), 
        Sequence(2),
        Target(Literal('b')), 
        Target(Literal('c')), 
        Target(Literal('d')), 
    ];

    assert_eq!(
        ast.sub_exp_indices(0),
        Some(vec![1, 2, 3, 4, 5]),
    );

    assert_eq!(
        ast.sub_exp_indices(2),
        Some(vec![3, 4]),
    );
}

#[test]
fn get_right_bound() {
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
        ast.right_bound(0),
        Some(6),
    );
    assert_eq!(
        ast.right_bound(2),
        Some(6),
    );
    assert_eq!(
        ast.right_bound(1),
        Some(2),
    );
    assert_eq!(
        ast.right_bound(4),
        Some(6),
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
        ast.right_bound(5),
        Some(9),
    );
    assert_eq!(
        ast.right_bound(4),
        Some(10),
    );
}
