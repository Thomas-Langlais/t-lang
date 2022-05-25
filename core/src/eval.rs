use std::ops::{Add, Div, Mul, Sub};

use crate::ast::Value;
use crate::exec::{InterpretedType, Machine, RTError, Result, __Result};
use crate::lexer::{CompType, LogicType};

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
            (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 + r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l + r as f64),
            // boolean handling
            (Value::Bool(l), Value::Bool(r)) => Value::Int(l as i64 + r as i64),
            (Value::Bool(l), Value::Int(r)) => Value::Int(l as i64 + r),
            (Value::Bool(l), Value::Float(r)) => Value::Float(if l { 1.0f64 + r } else { r }),
            (Value::Int(l), Value::Bool(r)) => Value::Int(l + r as i64),
            (Value::Float(l), Value::Bool(r)) => Value::Float(if r { l + 1.0f64 } else { l }),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
            (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 - r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l - r as f64),
            // boolean handling
            (Value::Bool(l), Value::Bool(r)) => Value::Int(l as i64 - r as i64),
            (Value::Bool(l), Value::Int(r)) => Value::Int(l as i64 - r),
            (Value::Bool(l), Value::Float(r)) => Value::Float(if l { 1.0f64 - r } else { -r }),
            (Value::Int(l), Value::Bool(r)) => Value::Int(l - r as i64),
            (Value::Float(l), Value::Bool(r)) => Value::Float(if r { l - 1.0f64 } else { l }),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
            (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 * r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l * r as f64),
            // boolean handling
            (Value::Bool(l), Value::Bool(r)) => Value::Int(l as i64 * r as i64),
            (Value::Bool(l), Value::Int(r)) => Value::Int(l as i64 * r),
            (Value::Bool(l), Value::Float(r)) => Value::Float(if l { r } else { 0.0 }),
            (Value::Int(l), Value::Bool(r)) => Value::Int(l * r as i64),
            (Value::Float(l), Value::Bool(r)) => Value::Float(if r { l } else { 0.0 }),
        }
    }
}

impl Div for Value {
    type Output = __Result<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() {
            return Err(RTError {
                name: "Divide by zero",
                details: "The right hand side of the division expression is 0",
            }
            .into());
        }

        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l / r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 / r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l / r as f64)),
            // boolean handling
            (Value::Bool(l), Value::Bool(r)) => Ok(Value::Int(l as i64 / r as i64)),
            (Value::Bool(l), Value::Int(r)) => Ok(Value::Int(l as i64 / r)),
            (Value::Bool(l), Value::Float(r)) => Ok(Value::Float(if l { 1.0f64 / r } else { 0.0 })),
            // it will always be the left side, because we handle div by zero above
            (Value::Int(l), Value::Bool(_)) => Ok(Value::Int(l)),
            (Value::Float(l), Value::Bool(_)) => Ok(Value::Float(l)),
        }
    }
}

impl Value {
    fn is_zero(&self) -> bool {
        match self {
            Self::Float(n) => n == &0.0,
            Self::Int(n) => n == &0,
            Self::Bool(n) => !n,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(n1), Self::Int(n2)) => n1 == n2,
            (Self::Float(n1), Self::Float(n2)) => n1 == n2,
            // because to_int_unchecked removes the float fraction representation,
            // it must ensure the float is it's ceiling before comparing
            (Self::Int(n1), Self::Float(n2)) => {
                n1 == &unsafe { n2.ceil().to_int_unchecked::<i64>() }
            }
            (Self::Float(n1), Self::Int(n2)) => {
                &(unsafe { n1.ceil().to_int_unchecked::<i64>() }) == n2
            }
            // boolean handling
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Bool(l), Value::Int(r)) => *l as i64 == *r,
            (Value::Bool(l), Value::Float(r)) => {
                *r == {
                    if *l {
                        1.0f64
                    } else {
                        0.0f64
                    }
                }
            }
            (Value::Int(l), Value::Bool(r)) => *l == *r as i64,
            (Value::Float(l), Value::Bool(r)) => {
                *l == {
                    if *r {
                        1.0f64
                    } else {
                        0.0f64
                    }
                }
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Int(n1), Self::Int(n2)) => n1.partial_cmp(n2),
            (Self::Float(n1), Self::Float(n2)) => n1.partial_cmp(n2),
            // because to_int_unchecked removes the float fraction representation,
            // it must ensure the float is it's ceiling before comparing
            (Self::Int(n1), Self::Float(n2)) => {
                let ceiled = n2.ceil();
                let i2 = &unsafe { ceiled.to_int_unchecked::<i64>() };
                n1.partial_cmp(i2)
            }
            (Self::Float(n1), Self::Int(n2)) => {
                let ceiled = n1.ceil();
                let i1 = &unsafe { ceiled.to_int_unchecked::<i64>() };
                i1.partial_cmp(n2)
            }
            // boolean handling
            (Value::Bool(l), Value::Bool(r)) => l.partial_cmp(r),
            (Value::Bool(l), Value::Int(r)) => (*l as i64).partial_cmp(r),
            (Value::Bool(l), Value::Float(r)) => {
                if *l {
                    1.0f64
                } else {
                    0.0f64
                }
            }
            .partial_cmp(r),
            (Value::Int(l), Value::Bool(r)) => l.partial_cmp(&(*r as i64)),
            (Value::Float(l), Value::Bool(r)) => l.partial_cmp({
                if *r {
                    &1.0f64
                } else {
                    &0.0f64
                }
            }),
        }
    }
}

impl Machine {
    pub(crate) async fn unary_arith_op(&self, rhs: Value, arith_type: char) -> Value {
        match arith_type {
            '+' => rhs,
            '-' => Value::Int(-1) * rhs,
            _ => unreachable!("A unary arithmetic operation can only be -/+"),
        }
    }

    pub(crate) async fn unary_logic_op(&self, rhs: bool, lgc_type: LogicType) -> Value {
        match lgc_type {
            LogicType::NOT => Value::Bool(!rhs),
            _ => unreachable!("A unary operator can only be !"),
        }
    }

    pub(crate) async fn term_arith_op(
        &self,
        arith_type: char,
        lhs: Value,
        rhs: Value,
    ) -> __Result<Value> {
        match arith_type {
            '+' => Ok(lhs + rhs),
            '-' => Ok(lhs - rhs),
            '*' => Ok(lhs * rhs),
            '/' => lhs / rhs,
            _ => unreachable!("unknown arithmetic operation"),
        }
    }

    pub(crate) async fn term_comp_op(
        &self,
        cmp_type: CompType,
        lhs: Value,
        rhs: Value,
    ) -> __Result<Value> {
        match cmp_type {
            CompType::EE => Ok(Value::Bool(lhs == rhs)),
            CompType::NE => Ok(Value::Bool(lhs != rhs)),
            CompType::GT => Ok(Value::Bool(lhs > rhs)),
            CompType::GTE => Ok(Value::Bool(lhs >= rhs)),
            CompType::LT => Ok(Value::Bool(lhs < rhs)),
            CompType::LTE => Ok(Value::Bool(lhs <= rhs)),
        }
    }

    pub(crate) async fn term_logic_op(
        &self,
        lgc_type: LogicType,
        lhs: bool,
        rhs: bool,
    ) -> __Result<Value> {
        match lgc_type {
            LogicType::AND => Ok(Value::Bool(lhs && rhs)),
            LogicType::OR => Ok(Value::Bool(lhs || rhs)),
            _ => unreachable!("Cannot interpret ! operation for a term"),
        }
    }
}
