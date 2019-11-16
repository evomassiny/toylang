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
}
use Value::*;

impl Value {
    pub fn pow(&self, other: &Self) -> Self {
        let self_float: f64 = self.into();
        let other_float: f64 = other.into();
        Num(self_float.powf(other_float))
    }
}

impl Into<bool> for &Value {
    fn into(self) -> bool {
        match *self {
            Str(ref s) => !s.is_empty(),
            Num(n) => n > 0.,
            Bool(b) => b,
            Function(_) => true,
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
            Function(_) => NAN,
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
            Function(_) => 0,
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
            Function(addr) => write!(f, "function at {}", addr),
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

