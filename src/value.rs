mod ops;

use super::FnCx;
use crate::ToType;
use naga::Expression;
use naga::Handle;
use std::marker::PhantomData;

pub struct Value<'a, T: ToType> {
    expr: Handle<Expression>,
    fn_cx: FnCx<'a>,
    val: PhantomData<fn() -> T>,
}

impl<'a, T: ToType> Value<'a, T> {
    pub fn expr(&self) -> Handle<Expression> {
        self.expr
    }

    pub(crate) fn with_expression<U: ToType>(&self, expression: Expression) -> Value<'a, U> {
        Value {
            expr: self.fn_cx.add_expression(expression),
            fn_cx: self.fn_cx.clone(),
            val: PhantomData,
        }
    }

    pub(crate) fn with_type<U: ToType>(&self) -> Value<'a, U> {
        Value {
            expr: self.expr,
            fn_cx: self.fn_cx.clone(),
            val: PhantomData,
        }
    }

    pub fn named(self, name: impl Into<String>) -> Self {
        self.fn_cx
            .with_function(|f| f.named_expressions.insert(self.expr, name.into()));
        self
    }

    pub(crate) fn new(expr: Expression, fn_cx: &FnCx<'a>) -> Self {
        Value {
            expr: fn_cx.add_expression(expr),
            fn_cx: fn_cx.clone(),
            val: PhantomData,
        }
    }
}
