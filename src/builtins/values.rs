use std::{
    f64::NAN,
    ops::{
        Add,
        Sub,
        Mul,
        Div,
        Rem,
        Not,
    },
};

/// The signature of a Native Function
pub type NativeFn = fn(&mut Vec<Value>) -> Value;

/// The ID of a given context
pub type ContextID = usize;

/// Describes the 2 kinds of Functions:
/// * Address => a location in the byte code
/// * Native => a rust function
#[derive(Clone)]
pub enum FnKind {
    Address(usize),
    Native(NativeFn),
}
impl std::fmt::Debug for FnKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Address(a) => write!(f, "Address {}", a),
            Self::Native(nfn) => write!(f, "Native {:?}", *nfn as *const ()),
        }
    }
}
impl std::cmp::PartialEq for FnKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Address(s), Self::Address(o)) => s == o,
            (Self::Native(s), Self::Native(o)) => (*s as *const ()) == (*o as *const ()),
            _ => false
        }
    }
}

#[derive(Debug,Clone,PartialEq)]
pub enum Value {
    /// any quoted string
    Str(String),
    /// int or float
    Num(f64),
    /// boolean `true` or `false`
    Bool(bool),
    /// A function
    Function(FnKind, ContextID),
    /// 
    Null,
    /// 
    Undefined,
}
use Value::*;

impl Value {
    /// raise `self` to the power `other`
    pub fn pow(&self, other: &Self) -> Self {
        let self_float: f64 = self.into();
        let other_float: f64 = other.into();
        Num(self_float.powf(other_float))
    }

    /// "==" operation
    pub fn is_equal(&self, other: &Self) -> Self {
        let equality = match (self, other) {
            (Null, Undefined) 
                | (Undefined, Null)
                | (Null, Null) 
                | (Undefined, Undefined) => true,
            // at least one is a string
            (Str(s), o) => s.to_string() == o.to_string(),
            (s, Str(o)) => s.to_string() == o.to_string(),
            // at least one is a number
            (Num(n), o) => {
                let other_float: f64 = o.into();
                *n == other_float
            },
            (s, Num(n)) => {
                let self_float: f64 = s.into();
                *n == self_float
            },
            // both are bool
            (Bool(s), Bool(o)) => *s == *o,
            // both are function
            (Function(fn_s, ..), Function(fn_o, ..)) => fn_s == fn_o,
            _ => false
        };
        Bool(equality)
    }
}

impl Into<bool> for &Value {
    fn into(self) -> bool {
        match *self {
            Str(ref s) => !s.is_empty(),
            Num(n) => n > 0.,
            Bool(b) => b,
            Function(..) => true,
            Null | Undefined => false,
        }
    }
}

impl Into<f64> for &Value {
    fn into(self) -> f64 {
        match *self {
            Num(num) => num,
            Bool(boolean) => if boolean { 1. } else { 0. },
            Null => 0.,
            Function(..) => NAN,
            Undefined => NAN,
            Str(ref s) => {
                //try to parse into f64
                match s.parse::<f64>() {
                    Ok(n) => n,
                    Err(_) => NAN,
                }
            }
        }
    }
}

impl Into<i32> for &Value {
    fn into(self) -> i32 {
        match *self {
            Num(num) => num as i32,
            Bool(boolean) => if boolean { 1 } else { 0 },
            Null => 0,
            Function(..) => 0,
            Undefined => 0,
            Str(ref s) => {
                //try to parse into i32
                match s.parse::<i32>() {
                    Ok(n) => n,
                    Err(_) => 0,
                }
            }
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Num(num) => write!(f, "{}", num),
            Bool(boolean) => write!(f, "{}", boolean),
            Null => write!(f, "null"),
            // WARNING ! the actual js function script should be here
            Function(FnKind::Address(addr), _) => write!(f, "function at {}", addr),
            Function(FnKind::Native(_), _) => write!(f, "native function"),
            Undefined => write!(f, "undefined"),
            Str(ref s) => write!(f, "{}", s),
        }
    }
}

impl Add for Value {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        match (self, other) {
            // if one of both is a string, concatenate them
            (Str(ref s), ref o) => Str(format!("{}{}", s.to_string(), o.to_string())),
            (ref s, Str(ref o)) => Str(format!("{}{}", s.to_string(), o.to_string())),
            // otherwise convert them into numbers and sum them
            (ref s, ref o) => {
                let s_float: f64 = s.into();
                let o_float: f64 = o.into();
                Num(s_float + o_float)
            },
        }
    }
}

impl Sub for Value {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        let self_float: f64 = (&self).into();
        let other_float: f64 = (&other).into();
        Num(self_float - other_float)
    }
}

impl Mul for Value {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        let self_float: f64 = (&self).into();
        let other_float: f64 = (&other).into();
        Num(self_float * other_float)
    }
}

impl Div for Value {
    type Output = Self;
    fn div(self, other: Self) -> Self {
        let self_float: f64 = (&self).into();
        let other_float: f64 = (&other).into();
        Num(self_float / other_float)
    }
}

impl Rem for Value {
    type Output = Self;
    fn rem(self, other: Self) -> Self {
        let self_float: f64 = (&self).into();
        let other_float: f64 = (&other).into();
        Num(self_float % other_float)
    }
}

impl Not for Value {
    type Output = Self;
    fn not(self) -> Self {
        let self_bool: bool = (&self).into();
        Bool(!self_bool)
    }
}

