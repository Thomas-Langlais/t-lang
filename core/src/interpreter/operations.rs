use std::ops::{Add, Div, Mul, Sub};

use super::{ExecutionContext, Interpret, InterpretedType, RTError, Result, TermNode, UnaryNode};
use crate::lexer::{CompType, LogicType};

impl Add for InterpretedType {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (InterpretedType::Int(l), InterpretedType::Int(r)) => InterpretedType::Int(l + r),
            (InterpretedType::Float(l), InterpretedType::Float(r)) => InterpretedType::Float(l + r),
            (InterpretedType::Int(l), InterpretedType::Float(r)) => {
                InterpretedType::Float(l as f64 + r)
            }
            (InterpretedType::Float(l), InterpretedType::Int(r)) => {
                InterpretedType::Float(l + r as f64)
            }
            // boolean handling
            (InterpretedType::Bool(l), InterpretedType::Bool(r)) => {
                InterpretedType::Int(l as i64 + r as i64)
            }
            (InterpretedType::Bool(l), InterpretedType::Int(r)) => {
                InterpretedType::Int(l as i64 + r)
            }
            (InterpretedType::Bool(l), InterpretedType::Float(r)) => {
                InterpretedType::Float(if l { 1.0f64 + r } else { r })
            }
            (InterpretedType::Int(l), InterpretedType::Bool(r)) => {
                InterpretedType::Int(l + r as i64)
            }
            (InterpretedType::Float(l), InterpretedType::Bool(r)) => {
                InterpretedType::Float(if r { l + 1.0f64 } else { l })
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
            (InterpretedType::Int(l), InterpretedType::Int(r)) => InterpretedType::Int(l - r),
            (InterpretedType::Float(l), InterpretedType::Float(r)) => InterpretedType::Float(l - r),
            (InterpretedType::Int(l), InterpretedType::Float(r)) => {
                InterpretedType::Float(l as f64 - r)
            }
            (InterpretedType::Float(l), InterpretedType::Int(r)) => {
                InterpretedType::Float(l - r as f64)
            }
            // boolean handling
            (InterpretedType::Bool(l), InterpretedType::Bool(r)) => {
                InterpretedType::Int(l as i64 - r as i64)
            }
            (InterpretedType::Bool(l), InterpretedType::Int(r)) => {
                InterpretedType::Int(l as i64 - r)
            }
            (InterpretedType::Bool(l), InterpretedType::Float(r)) => {
                InterpretedType::Float(if l { 1.0f64 - r } else { -r })
            }
            (InterpretedType::Int(l), InterpretedType::Bool(r)) => {
                InterpretedType::Int(l - r as i64)
            }
            (InterpretedType::Float(l), InterpretedType::Bool(r)) => {
                InterpretedType::Float(if r { l - 1.0f64 } else { l })
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
            (InterpretedType::Int(l), InterpretedType::Int(r)) => InterpretedType::Int(l * r),
            (InterpretedType::Float(l), InterpretedType::Float(r)) => InterpretedType::Float(l * r),
            (InterpretedType::Int(l), InterpretedType::Float(r)) => {
                InterpretedType::Float(l as f64 * r)
            }
            (InterpretedType::Float(l), InterpretedType::Int(r)) => {
                InterpretedType::Float(l * r as f64)
            }
            // boolean handling
            (InterpretedType::Bool(l), InterpretedType::Bool(r)) => {
                InterpretedType::Int(l as i64 * r as i64)
            }
            (InterpretedType::Bool(l), InterpretedType::Int(r)) => {
                InterpretedType::Int(l as i64 * r)
            }
            (InterpretedType::Bool(l), InterpretedType::Float(r)) => {
                InterpretedType::Float(if l { r } else { 0.0 })
            }
            (InterpretedType::Int(l), InterpretedType::Bool(r)) => {
                InterpretedType::Int(l * r as i64)
            }
            (InterpretedType::Float(l), InterpretedType::Bool(r)) => {
                InterpretedType::Float(if r { l } else { 0.0 })
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
            });
        }

        match (self, rhs) {
            (InterpretedType::Int(l), InterpretedType::Int(r)) => Ok(InterpretedType::Int(l / r)),
            (InterpretedType::Float(l), InterpretedType::Float(r)) => {
                Ok(InterpretedType::Float(l / r))
            }
            (InterpretedType::Int(l), InterpretedType::Float(r)) => {
                Ok(InterpretedType::Float(l as f64 / r))
            }
            (InterpretedType::Float(l), InterpretedType::Int(r)) => {
                Ok(InterpretedType::Float(l / r as f64))
            }
            // boolean handling
            (InterpretedType::Bool(l), InterpretedType::Bool(r)) => {
                Ok(InterpretedType::Int(l as i64 / r as i64))
            }
            (InterpretedType::Bool(l), InterpretedType::Int(r)) => {
                Ok(InterpretedType::Int(l as i64 / r))
            }
            (InterpretedType::Bool(l), InterpretedType::Float(r)) => {
                Ok(InterpretedType::Float(if l { 1.0f64 / r } else { 0.0 }))
            }
            // it will always be the left side, because we handle div by zero above
            (InterpretedType::Int(l), InterpretedType::Bool(_)) => Ok(InterpretedType::Int(l)),
            (InterpretedType::Float(l), InterpretedType::Bool(_)) => Ok(InterpretedType::Float(l)),
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
            Self::Float(n) => n == &0.0,
            Self::Int(n) => n == &0,
            Self::Bool(n) => !n,
            Self::Break | Self::Continue => {
                unreachable!("break, and continues are never to be handledx")
            }
        }
    }
}

impl PartialEq for InterpretedType {
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
            (InterpretedType::Bool(l), InterpretedType::Bool(r)) => l == r,
            (InterpretedType::Bool(l), InterpretedType::Int(r)) => *l as i64 == *r,
            (InterpretedType::Bool(l), InterpretedType::Float(r)) => {
                *r == {
                    if *l {
                        1.0f64
                    } else {
                        0.0f64
                    }
                }
            }
            (InterpretedType::Int(l), InterpretedType::Bool(r)) => *l == *r as i64,
            (InterpretedType::Float(l), InterpretedType::Bool(r)) => {
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
            (InterpretedType::Bool(l), InterpretedType::Bool(r)) => l.partial_cmp(r),
            (InterpretedType::Bool(l), InterpretedType::Int(r)) => (*l as i64).partial_cmp(r),
            (InterpretedType::Bool(l), InterpretedType::Float(r)) => {
                if *l {
                    1.0f64
                } else {
                    0.0f64
                }
            }
            .partial_cmp(r),
            (InterpretedType::Int(l), InterpretedType::Bool(r)) => l.partial_cmp(&(*r as i64)),
            (InterpretedType::Float(l), InterpretedType::Bool(r)) => l.partial_cmp({
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

impl TermNode {
    pub fn arith_op(&self, arith_type: char, lhs: InterpretedType, rhs: InterpretedType) -> Result {
        match arith_type {
            '+' => Ok(lhs + rhs),
            '-' => Ok(lhs - rhs),
            '*' => Ok(lhs * rhs),
            '/' => lhs / rhs,
            _ => unreachable!("unknown arithmetic operation"),
        }
    }

    pub fn comp_op(
        &self,
        cmp_type: CompType,
        lhs: InterpretedType,
        rhs: InterpretedType,
    ) -> Result {
        match cmp_type {
            CompType::EE => Ok(InterpretedType::Bool(lhs == rhs)),
            CompType::NE => Ok(InterpretedType::Bool(lhs != rhs)),
            CompType::GT => Ok(InterpretedType::Bool(lhs > rhs)),
            CompType::GTE => Ok(InterpretedType::Bool(lhs >= rhs)),
            CompType::LT => Ok(InterpretedType::Bool(lhs < rhs)),
            CompType::LTE => Ok(InterpretedType::Bool(lhs <= rhs)),
        }
    }

    pub fn logic_op(&self, lgc_type: LogicType, lhs: bool, rhs: bool) -> Result {
        match lgc_type {
            LogicType::AND => Ok(InterpretedType::Bool(lhs && rhs)),
            LogicType::OR => Ok(InterpretedType::Bool(lhs || rhs)),
            _ => unreachable!("Cannot interpret ! operation for a term"),
        }
    }
}

impl UnaryNode {
    pub fn arith_op(&self, arith_type: char, context: &mut ExecutionContext) -> Result {
        match arith_type {
            '+' => self.node.interpret(context),
            '-' => {
                let rhs = self.node.interpret(context)?;
                Ok(InterpretedType::Int(-1) * rhs)
            }
            _ => unreachable!("A unary arithmetic operation can only be -/+"),
        }
    }

    pub fn logic_op(&self, lgc_type: LogicType, context: &mut ExecutionContext) -> Result {
        match lgc_type {
            LogicType::NOT => {
                let rhs = self.node.interpret(context)?;
                Ok(InterpretedType::Bool(!bool::from(rhs)))
            }
            _ => unreachable!("A unary operator can only be !"),
        }
    }
}
