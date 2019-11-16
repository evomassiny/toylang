use std::ops::Add;

#[derive(Debug,Clone,PartialEq)]
pub enum Value {
    /// any quoted string
    Str(String),
    /// int or float
    Num(f64),
    /// boolean `true` or `false`
    Bool(bool),
    /// A function address
    Function(usize),
    /// 
    Null,
    /// 
    Undefined,
    /// 
    NaN,
}

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        use Value::*;
        // This is awful, there must be a better way
        match self {
            Str(s) => {
                match other {
                    Str(s_other) => Str(format!("{}{}", s, s_other)),
                    Num(n) => Str(format!("{}{}", s, n)),
                    Bool(b) => {
                        let bool_repr = if b { "true" } else { "false" };
                        Str(format!("{}{}", s, bool_repr))
                    },
                    // the supposed behaviour is to append the function script string !
                    // WTF js !?
                    Function(_) => unimplemented!(),
                    Null => Str(format!("{}null", s)), 
                    NaN => Str(format!("{}NaN", s)), 
                    Undefined => Str(format!("{}undefined", s)),
                }
            },
            Num(n) => {
                match other {
                    Str(s) => Str(format!("{}{}", n, s)),
                    Num(n_other) => Num(n + n_other),
                    // behave as if true = 1, bool = 0
                    Bool(b) =>  if b { Num(n + 1.) } else { Num(n) },
                    Function(_) => unimplemented!(),
                    NaN => NaN,
                    Null => Num(n),
                    Undefined => NaN,
                }
            },
            Bool(b) => {
                match other {
                    Str(s) => {
                        let bool_repr = if b { "true" } else { "false" };
                        Str(format!("{}{}", bool_repr, s))
                    },
                    Num(n) => {
                        let self_value: f64 = if b { 1. } else { 0. };
                        Num(n + self_value)
                    },
                    Bool(b_other) => {
                        let self_value: f64 = if b { 1. } else { 0. };
                        let other_value: f64 = if b_other { 1. } else { 0. };
                        Num(self_value + other_value)
                    },
                    Function(_) => unimplemented!(),
                    NaN => NaN,
                    Null => if b { Num(1.) } else { Num(0.) },
                    Undefined => NaN,
                }
            },
            Function(_) => unimplemented!(),
            Null => {
                match other {
                    Str(s) => Str(format!("null{}", s)),
                    Num(n) => Num(n),
                    Bool(b) => Num(if b { 1. } else { 0. }),
                    Function(_) => unimplemented!(),
                    NaN => NaN,
                    Null => Num(0.),
                    Undefined => NaN,
                }
            },
            NaN => {
                match other {
                    Str(s) => Str(format!("NaN{}", s)),
                    Function(_) => unimplemented!(),
                    _ => NaN
                }
            },
            Undefined => {
                match other {
                    Str(s) => Str(format!("Undefined{}", s)),
                    Function(_) => unimplemented!(),
                    _ => NaN
                }
            },
        }
    }
}

