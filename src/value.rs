mod ops;

use super::FnCx;
use crate::ToType;
use naga::Constant;
use naga::Expression;
use naga::Handle;
use std::marker::PhantomData;

#[derive(Copy, Clone)]
enum ValueBacking {
    Expression(Handle<Expression>),
    // Used only sparingly, as monomorphises entire program; generally only set on entry points
    Constant(Handle<Constant>),
}

impl ValueBacking {
    fn expression(&self, cx: &FnCx) -> Handle<Expression> {
        match self {
            ValueBacking::Expression(expr) => *expr,
            ValueBacking::Constant(const_) => cx.add_expression(Expression::Constant(*const_)),
        }
    }
}

#[derive(Clone)]
pub struct Value<'a, T: ToType> {
    expr: ValueBacking,
    fn_cx: FnCx<'a>,
    val: PhantomData<fn() -> T>,
}

impl<'a, T: ToType> Value<'a, T> {
    pub fn expr(&self) -> Handle<Expression> {
        self.expr.expression(&self.fn_cx)
    }

    pub(crate) fn from_expr_handle(expr: Handle<Expression>, fn_cx: &FnCx<'a>) -> Self {
        Value {
            expr: ValueBacking::Expression(expr),
            fn_cx: fn_cx.clone(),
            val: PhantomData,
        }
    }

    pub(crate) fn from_const_handle(const_: Handle<Constant>, fn_cx: &FnCx<'a>) -> Self {
        Value {
            expr: ValueBacking::Constant(const_),
            fn_cx: fn_cx.clone(),
            val: PhantomData,
        }
    }

    pub(crate) fn with_expression<U: ToType>(&self, expression: Expression) -> Value<'a, U> {
        Value::<U>::from_expr_handle(self.fn_cx.add_expression(expression), &self.fn_cx)
    }

    pub(crate) fn with_type<U: ToType>(&self) -> Value<'a, U> {
        Value {
            expr: self.expr,
            fn_cx: self.fn_cx.clone(),
            val: PhantomData,
        }
    }

    pub fn named(self, name: impl Into<String>) -> Self {
        let expr = self.expr();
        self.fn_cx
            .with_function(|f| f.named_expressions.insert(expr, name.into()));
        Self::from_expr_handle(expr, &self.fn_cx)
    }

    pub fn realise(self) -> Self {
        let expr = self.expr();
        Self::from_expr_handle(expr, &self.fn_cx)
    }

    pub(crate) fn new(expr: Expression, fn_cx: &FnCx<'a>) -> Self {
        Self::from_expr_handle(fn_cx.add_expression(expr), fn_cx)
    }
}
