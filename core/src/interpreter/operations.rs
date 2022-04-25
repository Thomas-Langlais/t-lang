use std::ops::{Add, Div, Mul, Sub};

use super::{
    DivideByZeroError, ExecutionContext, Interpret, InterpretedType, InterpreterError,
    InterpreterResult, TermNode, UnaryNode,
};
use crate::lexer::{CompType, LogicType};

pub struct OpDivByZeroError {
    err_type: &'static str,
    details: &'static str,
}

impl<'a> OpDivByZeroError {
    pub fn into(self, context: &ExecutionContext) -> InterpreterError {
        let (start, end) = context.current_pos;
        InterpreterError::DivideByZero(DivideByZeroError {
            name: self.err_type,
            details: self.details,
            source: context.source_text.clone(),
            start,
            end,
            line: 0,
        })
    }
}

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
        }
    }
}

impl Div for InterpretedType {
    type Output = Result<InterpretedType, OpDivByZeroError>;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() {
            return Err(OpDivByZeroError {
                err_type: "Divide by zero",
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
        }
    }
}

impl InterpretedType {
    fn is_zero(&self) -> bool {
        match self {
            Self::Float(n) => n == &0.0,
            Self::Int(n) => n == &0,
        }
    }
}

impl PartialEq for InterpretedType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(n1), Self::Int(n2)) => n1 == n2,
            (Self::Float(n1), Self::Float(n2)) => n1 == n2,
            (Self::Int(n1), Self::Float(n2)) => n1 == &unsafe { n2.to_int_unchecked::<i64>() },
            (Self::Float(n1), Self::Int(n2)) => &(unsafe { n1.to_int_unchecked::<i64>() }) == n2,
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
        }
    }
}

impl TermNode {
    pub fn arith_op(
        &self,
        arith_type: char,
        lhs: InterpretedType,
        rhs: InterpretedType,
        context: &ExecutionContext,
    ) -> InterpreterResult {
        match arith_type {
            '+' => Ok(lhs + rhs),
            '-' => Ok(lhs - rhs),
            '*' => Ok(lhs * rhs),
            '/' => match lhs / rhs {
                Ok(res) => Ok(res),
                Err(err) => Err(err.into(context)),
            },
            _ => unreachable!("unknown arithmetic operation"),
        }
    }

    pub fn comp_op(
        &self,
        cmp_type: CompType,
        lhs: InterpretedType,
        rhs: InterpretedType,
    ) -> InterpreterResult {
        match cmp_type {
            CompType::EE => Ok(InterpretedType::Int(i64::from(lhs == rhs))),
            CompType::NE => Ok(InterpretedType::Int(i64::from(lhs != rhs))),
            CompType::GT => Ok(InterpretedType::Int(i64::from(lhs > rhs))),
            CompType::GTE => Ok(InterpretedType::Int(i64::from(lhs >= rhs))),
            CompType::LT => Ok(InterpretedType::Int(i64::from(lhs < rhs))),
            CompType::LTE => Ok(InterpretedType::Int(i64::from(lhs <= rhs))),
        }
    }

    pub fn logic_op(&self, lgc_type: LogicType, lhs: bool, rhs: bool) -> InterpreterResult {
        match lgc_type {
            LogicType::AND => Ok(InterpretedType::Int(i64::from(lhs && rhs))),
            LogicType::OR => Ok(InterpretedType::Int(i64::from(lhs || rhs))),
            _ => unreachable!("Cannot interpret ! operation for a term"),
        }
    }
}

impl UnaryNode {
    pub fn arith_op(&self, arith_type: char, context: &mut ExecutionContext) -> InterpreterResult {
        match arith_type {
            '+' => self.node.interpret(context),
            '-' => {
                let rhs = self.node.interpret(context)?;
                context.current_pos = self.pos;
                Ok(InterpretedType::Int(-1) * rhs)
            }
            _ => unreachable!("A unary arithmetic operation can only be -/+"),
        }
    }

    pub fn logic_op(&self, lgc_type: LogicType, context: &mut ExecutionContext) -> InterpreterResult {
        match lgc_type {
            LogicType::NOT => {
                let rhs = self.node.interpret(context)?;
                context.current_pos = self.pos;
                Ok(InterpretedType::Int(i64::from(!bool::from(rhs))))
            }
            _ => unreachable!("A unary operator can only be !"),
        }
    }
}
