pub mod entry_point;
mod ops;

use super::FnCx;
use crate::ToType;
use naga::Constant;
use naga::Expression;
use naga::Handle;
pub use ops::ToExpr;
use std::marker::PhantomData;

#[derive(Copy, Clone)]
enum ValueBacking {
    // TODO: Make Value only for Expressions, add different type Const for constants
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

/// Represents the result of an expression within a shader function
pub struct Value<'a, T: ToType> {
    expr: ValueBacking,
    fn_cx: FnCx<'a>,
    marker: PhantomData<fn() -> T>,
}

// Need to use manual implementations because
impl<'a, T: ToType> Clone for Value<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: ToType> Copy for Value<'a, T> {}

impl<'a, T: ToType> Value<'a, T> {
    pub fn expr(&self) -> Handle<Expression> {
        self.expr.expression(&self.fn_cx)
    }

    pub(crate) fn from_expr_handle(expr: Handle<Expression>, fn_cx: FnCx<'a>) -> Self {
        Value {
            expr: ValueBacking::Expression(expr),
            fn_cx,
            marker: PhantomData,
        }
    }

    pub(crate) fn with_cx<'o>(self, cx: FnCx<'o>) -> Value<'o, T> {
        Value {
            expr: self.expr,
            fn_cx: cx,
            marker: PhantomData,
        }
    }

    pub(crate) fn from_const_handle(const_: Handle<Constant>, fn_cx: FnCx<'a>) -> Self {
        Value {
            expr: ValueBacking::Constant(const_),
            fn_cx,
            marker: PhantomData,
        }
    }

    pub(crate) fn with_expression<U: ToType>(&self, expression: Expression) -> Value<'a, U> {
        Value::<U>::from_expr_handle(self.fn_cx.add_expression(expression), self.fn_cx)
    }

    pub(crate) fn with_type<U: ToType>(&self) -> Value<'a, U> {
        Value {
            expr: self.expr,
            fn_cx: self.fn_cx,
            marker: PhantomData,
        }
    }

    pub fn named(self, name: impl Into<String>) -> Self {
        let expr = self.expr();
        self.fn_cx
            .with_function(|f| f.named_expressions.insert(expr, name.into()));
        Self::from_expr_handle(expr, self.fn_cx)
    }

    pub fn realise(self) -> Self {
        let expr = self.expr();
        Self::from_expr_handle(expr, self.fn_cx)
    }

    pub(crate) fn new(expr: Expression, fn_cx: FnCx<'a>) -> Self {
        Self::from_expr_handle(fn_cx.add_expression(expr), fn_cx)
    }
}

pub trait WrappingValue: 'static + Sized + ToType {
    type Inner: ToType;
}

impl<'a, T: WrappingValue> Value<'a, T> {
    // TODO: Have this be `Deref`, because the different value types have the same layout
    pub fn inner(self) -> Value<'a, T::Inner> {
        self.with_type()
    }
}

#[macro_export]
macro_rules! wrapper {
    ($(#[$attrs: meta])* struct $type: ident: $inner: ty) => {
        $(#[$attrs])*
        pub struct $type;
        impl $crate::value::WrappingValue for $type {
            type Inner = $inner;
        }


        impl $crate::ToType for $type {
            fn naga_ty_inner(_: &mut $crate::TypeRegistry) -> $crate::naga::TypeInner {
                unimplemented!();
            }

            fn type_handle(registry: &mut $crate::TypeRegistry) -> $crate::naga::Handle<naga::Type> {
                registry.register_type::<<Self as $crate::value::WrappingValue>::Inner>()
            }
        }
    };
}
