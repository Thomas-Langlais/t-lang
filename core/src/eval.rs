use std::ops::{Add, Div, Mul, Sub};

use crate::ast::Value;
use crate::lexer::{CompType, LogicType};
use crate::exec::{InterpretedType, RTError, Result, Machine};

impl Add for InterpretedType {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Int(r))) => InterpretedType::Value(Value::Int(l + r)),
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Float(r))) => InterpretedType::Value(Value::Float(l + r)),
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Float(r))) => {
                InterpretedType::Value(Value::Float(l as f64 + r))
            }
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Int(r))) => {
                InterpretedType::Value(Value::Float(l + r as f64))
            }
            // boolean handling
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Int(l as i64 + r as i64))
            }
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Int(r))) => {
                InterpretedType::Value(Value::Int(l as i64 + r))
            }
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Float(r))) => {
                InterpretedType::Value(Value::Float(if l { 1.0f64 + r } else { r }))
            }
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Int(l + r as i64))
            }
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Float(if r { l + 1.0f64 } else { l }))
            }
            (InterpretedType::Break, _)
            | (_, InterpretedType::Break)
            | (InterpretedType::Continue, _)
            | (_, InterpretedType::Continue) => {
                unreachable!("breaks, and continue are never to be handled")
            }
        }
    }
}

impl Sub for InterpretedType {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Int(r))) => InterpretedType::Value(Value::Int(l - r)),
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Float(r))) => InterpretedType::Value(Value::Float(l - r)),
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Float(r))) => {
                InterpretedType::Value(Value::Float(l as f64 - r))
            }
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Int(r))) => {
                InterpretedType::Value(Value::Float(l - r as f64))
            }
            // boolean handling
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Int(l as i64 - r as i64))
            }
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Int(r))) => {
                InterpretedType::Value(Value::Int(l as i64 - r))
            }
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Float(r))) => {
                InterpretedType::Value(Value::Float(if l { 1.0f64 - r } else { -r }))
            }
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Int(l - r as i64))
            }
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Float(if r { l - 1.0f64 } else { l }))
            }
            (InterpretedType::Break, _)
            | (_, InterpretedType::Break)
            | (InterpretedType::Continue, _)
            | (_, InterpretedType::Continue) => {
                unreachable!("breaks, and continue are never to be handled")
            }
        }
    }
}

impl Mul for InterpretedType {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Int(r))) => InterpretedType::Value(Value::Int(l * r)),
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Float(r))) => InterpretedType::Value(Value::Float(l * r)),
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Float(r))) => {
                InterpretedType::Value(Value::Float(l as f64 * r))
            }
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Int(r))) => {
                InterpretedType::Value(Value::Float(l * r as f64))
            }
            // boolean handling
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Int(l as i64 * r as i64))
            }
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Int(r))) => {
                InterpretedType::Value(Value::Int(l as i64 * r))
            }
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Float(r))) => {
                InterpretedType::Value(Value::Float(if l { r } else { 0.0 }))
            }
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Int(l * r as i64))
            }
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Bool(r))) => {
                InterpretedType::Value(Value::Float(if r { l } else { 0.0 }))
            }
            (InterpretedType::Break, _)
            | (_, InterpretedType::Break)
            | (InterpretedType::Continue, _)
            | (_, InterpretedType::Continue) => {
                unreachable!("breaks, and continue are never to be handled")
            }
        }
    }
}

impl Div for InterpretedType {
    type Output = Result;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() {
            return Err(RTError {
                name: "Divide by zero",
                details: "The right hand side of the division expression is 0",
            }.into());
        }

        match (self, rhs) {
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Int(r))) => Ok(InterpretedType::Value(Value::Int(l / r))),
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Float(r))) => {
                Ok(InterpretedType::Value(Value::Float(l / r)))
            }
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Float(r))) => {
                Ok(InterpretedType::Value(Value::Float(l as f64 / r)))
            }
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Int(r))) => {
                Ok(InterpretedType::Value(Value::Float(l / r as f64)))
            }
            // boolean handling
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Bool(r))) => {
                Ok(InterpretedType::Value(Value::Int(l as i64 / r as i64)))
            }
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Int(r))) => {
                Ok(InterpretedType::Value(Value::Int(l as i64 / r)))
            }
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Float(r))) => {
                Ok(InterpretedType::Value(Value::Float(if l { 1.0f64 / r } else { 0.0 })))
            }
            // it will always be the left side, because we handle div by zero above
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Bool(_))) => Ok(InterpretedType::Value(Value::Int(l))),
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Bool(_))) => Ok(InterpretedType::Value(Value::Float(l))),
            (InterpretedType::Break, _)
            | (_, InterpretedType::Break)
            | (InterpretedType::Continue, _)
            | (_, InterpretedType::Continue) => {
                unreachable!("breaks, and continue are never to be handled")
            }
        }
    }
}

impl InterpretedType {
    fn is_zero(&self) -> bool {
        match self {
            Self::Value(Value::Float(n)) => n == &0.0,
            Self::Value(Value::Int(n)) => n == &0,
            Self::Value(Value::Bool(n)) => !n,
            Self::Break | Self::Continue => {
                unreachable!("break, and continues are never to be handledx")
            }
        }
    }
}

impl PartialEq for InterpretedType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Value(Value::Int(n1)), Self::Value(Value::Int(n2))) => n1 == n2,
            (Self::Value(Value::Float(n1)), Self::Value(Value::Float(n2))) => n1 == n2,
            // because to_int_unchecked removes the float fraction representation,
            // it must ensure the float is it's ceiling before comparing
            (Self::Value(Value::Int(n1)), Self::Value(Value::Float(n2))) => {
                n1 == &unsafe { n2.ceil().to_int_unchecked::<i64>() }
            }
            (Self::Value(Value::Float(n1)), Self::Value(Value::Int(n2))) => {
                &(unsafe { n1.ceil().to_int_unchecked::<i64>() }) == n2
            }
            // boolean handling
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Bool(r))) => l == r,
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Int(r))) => *l as i64 == *r,
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Float(r))) => {
                *r == {
                    if *l {
                        1.0f64
                    } else {
                        0.0f64
                    }
                }
            }
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Bool(r))) => *l == *r as i64,
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Bool(r))) => {
                *l == {
                    if *r {
                        1.0f64
                    } else {
                        0.0f64
                    }
                }
            }
            (InterpretedType::Break, _)
            | (_, InterpretedType::Break)
            | (InterpretedType::Continue, _)
            | (_, InterpretedType::Continue) => {
                unreachable!("breaks, and continue are never to be handled")
            }
        }
    }
}

impl PartialOrd for InterpretedType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Value(Value::Int(n1)), Self::Value(Value::Int(n2))) => n1.partial_cmp(n2),
            (Self::Value(Value::Float(n1)), Self::Value(Value::Float(n2))) => n1.partial_cmp(n2),
            // because to_int_unchecked removes the float fraction representation,
            // it must ensure the float is it's ceiling before comparing
            (Self::Value(Value::Int(n1)), Self::Value(Value::Float(n2))) => {
                let ceiled = n2.ceil();
                let i2 = &unsafe { ceiled.to_int_unchecked::<i64>() };
                n1.partial_cmp(i2)
            }
            (Self::Value(Value::Float(n1)), Self::Value(Value::Int(n2))) => {
                let ceiled = n1.ceil();
                let i1 = &unsafe { ceiled.to_int_unchecked::<i64>() };
                i1.partial_cmp(n2)
            }
            // boolean handling
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Bool(r))) => l.partial_cmp(r),
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Int(r))) => (*l as i64).partial_cmp(r),
            (InterpretedType::Value(Value::Bool(l)), InterpretedType::Value(Value::Float(r))) => {
                if *l {
                    1.0f64
                } else {
                    0.0f64
                }
            }
            .partial_cmp(r),
            (InterpretedType::Value(Value::Int(l)), InterpretedType::Value(Value::Bool(r))) => l.partial_cmp(&(*r as i64)),
            (InterpretedType::Value(Value::Float(l)), InterpretedType::Value(Value::Bool(r))) => l.partial_cmp({
                if *r {
                    &1.0f64
                } else {
                    &0.0f64
                }
            }),
            (InterpretedType::Break, _)
            | (_, InterpretedType::Break)
            | (InterpretedType::Continue, _)
            | (_, InterpretedType::Continue) => {
                unreachable!("breaks, and continue are never to be handled")
            }
        }
    }
}

impl Machine<'_> {
    pub(crate) async fn unary_arith_op(&self, input: Result, arith_type: char) -> Result {
        match arith_type {
            '+' => input,
            '-' => {
                let rhs = input?;
                Ok(InterpretedType::Value(Value::Int(-1)) * rhs)
            }
            _ => unreachable!("A unary arithmetic operation can only be -/+"),
        }
    }

    pub(crate) async fn unary_logic_op(&self, input: Result, lgc_type: LogicType) -> Result {
        match lgc_type {
            LogicType::NOT => {
                let rhs = input?;
                Ok(InterpretedType::Value(Value::Bool(!bool::from(rhs))))
            }
            _ => unreachable!("A unary operator can only be !"),
        }
    }
    
    pub(crate) async fn term_arith_op(&self, arith_type: char, lhs: InterpretedType, rhs: InterpretedType) -> Result {
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
        lhs: InterpretedType,
        rhs: InterpretedType,
    ) -> Result {
        match cmp_type {
            CompType::EE => Ok(InterpretedType::Value(Value::Bool(lhs == rhs))),
            CompType::NE => Ok(InterpretedType::Value(Value::Bool(lhs != rhs))),
            CompType::GT => Ok(InterpretedType::Value(Value::Bool(lhs > rhs))),
            CompType::GTE => Ok(InterpretedType::Value(Value::Bool(lhs >= rhs))),
            CompType::LT => Ok(InterpretedType::Value(Value::Bool(lhs < rhs))),
            CompType::LTE => Ok(InterpretedType::Value(Value::Bool(lhs <= rhs))),
        }
    }

    pub(crate) async fn term_logic_op(&self, lgc_type: LogicType, lhs: bool, rhs: bool) -> Result {
        match lgc_type {
            LogicType::AND => Ok(InterpretedType::Value(Value::Bool(lhs && rhs))),
            LogicType::OR => Ok(InterpretedType::Value(Value::Bool(lhs || rhs))),
            _ => unreachable!("Cannot interpret ! operation for a term"),
        }
    }
}
