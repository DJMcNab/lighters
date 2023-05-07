use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use glam::{IVec2, IVec3, IVec4, UVec2, UVec3, UVec4, Vec2, Vec3, Vec4};
use naga::{BinaryOperator, Expression, Handle};

use crate::{
    types::{PointerType, ToConstant},
    BlockContext, FnCx, ToType, Value,
};

/// Can be added/minused/multiplied/divided
pub trait ValueMathOp<Rhs = Self>: ToType {
    type Result: ToType;
}
// TODO:
// /// Can be compared (Eq, )
// trait ValueCmpOp<Rhs = Self>: ToType {
//     type Result: ToType;
// }
/// Can be unary negated
trait ValueNegate {}

/// Can be bit complemented, OR, AND, XOR, NEG (~ in WGSL, ! in Rust)
trait ValueBitOp<Rhs = Self>: ToType {
    type Result: ToType;
}

/// Can be shifted
trait ValueShiftOp<Rhs = u32>: ToType {
    type Result: ToType;
}

impl ValueMathOp for u32 {
    type Result = Self;
}
impl ValueMathOp for i32 {
    type Result = Self;
}
impl ValueMathOp for f32 {
    type Result = Self;
}

impl ValueNegate for i32 {}
impl ValueNegate for f32 {}

pub trait Comparable: ToType {}

impl Comparable for i32 {}
impl Comparable for u32 {}
impl Comparable for f32 {}

pub trait ToExpr {
    type T: ToType;
    fn get_expr(self, cx: FnCx<'_>) -> Handle<Expression>;
}

impl<'a, T: ToType> ToExpr for Value<'_, T> {
    type T = T;

    fn get_expr(self, _cx: FnCx<'_>) -> Handle<Expression> {
        self.expr()
    }
}

impl<'a, T: ToConstant + ToType> ToExpr for T {
    type T = T;

    fn get_expr(self, cx: FnCx<'_>) -> Handle<Expression> {
        cx.const_(self).expr()
    }
}

impl<'a, T: Comparable> Value<'a, T> {
    pub fn le<U: ToExpr<T = T>>(&self, other: U) -> Value<'a, bool> {
        self.with_expression(Expression::Binary {
            op: BinaryOperator::LessEqual,
            left: self.expr(),
            right: other.get_expr(self.fn_cx),
        })
    }
    pub fn lt<U: ToExpr<T = T>>(&self, other: U) -> Value<'a, bool> {
        self.with_expression(Expression::Binary {
            op: BinaryOperator::Less,
            left: self.expr(),
            right: other.get_expr(self.fn_cx),
        })
    }
    pub fn eq<U: ToExpr<T = T>>(&self, other: U) -> Value<'a, bool> {
        self.with_expression(Expression::Binary {
            op: BinaryOperator::Equal,
            left: self.expr(),
            right: other.get_expr(self.fn_cx),
        })
    }
    pub fn neq<U: ToExpr<T = T>>(&self, other: U) -> Value<'a, bool> {
        self.with_expression(Expression::Binary {
            op: BinaryOperator::NotEqual,
            left: self.expr(),
            right: other.get_expr(self.fn_cx),
        })
    }
    pub fn gt<U: ToExpr<T = T>>(&self, other: U) -> Value<'a, bool> {
        self.with_expression(Expression::Binary {
            op: BinaryOperator::Greater,
            left: self.expr(),
            right: other.get_expr(self.fn_cx),
        })
    }
    pub fn ge<U: ToExpr<T = T>>(&self, other: U) -> Value<'a, bool> {
        self.with_expression(Expression::Binary {
            op: BinaryOperator::GreaterEqual,
            left: self.expr(),
            right: other.get_expr(self.fn_cx),
        })
    }
}

macro_rules! vector {
    ($kind: ty: $component: ty, negates ) => {
        impl ValueNegate for $kind {}
        vector!($kind: $component);
    };
    ($kind: ty: $component: ty ) => {
        // TODO: These are not true, because of naga's rules.
        // We should probably just make these methods.
        // `glam` does support these operations in the "splatting" form
        // impl ValueMathOp<$component> for $kind {}
        // impl ValueMathOp<$kind> for $component {}
        impl ValueMathOp<$kind> for $kind {
            type Result = Self;
        }
    };
}

vector!(Vec2: f32, negates);
vector!(Vec3: f32, negates);
vector!(Vec4: f32, negates);

vector!(IVec2: i32, negates);
vector!(IVec3: i32, negates);
vector!(IVec4: i32, negates);

vector!(UVec2: u32);
vector!(UVec3: u32);
vector!(UVec4: u32);

macro_rules! binop {
    ($trait: ident when $bound: ident: $operation: ident, $fn_name: ident, $op: tt) => {
        // Combinations needed: l + r; &l + r; l + &r; &l + &r; l + const; &l + const

        // This is the base implementation, other implementations all call into this
        impl<'l,'r, L: ToType + $bound<R>, R: ToType + ToConstant> $trait<&Value<'r, R>>
            for &Value<'l, L>
        {
            type Output = Value<'l, L::Result>;

            fn $fn_name(self, rhs: &Value<'r, R>) -> Self::Output {
                self.with_expression(Expression::Binary {
                    op: naga::BinaryOperator::$operation,
                    left: self.expr(),
                    right: rhs.expr(),
                })
            }
        }
        impl<'l, 'r, L: ToType + $bound<R>, R: ToType + ToConstant> $trait<Value<'r, R>>
            for &Value<'l, L>
        {
            type Output = Value<'l, L::Result>;

            fn $fn_name(self, rhs: Value<'r, R>) -> Self::Output {
                self $op &rhs
            }
        }
        impl<'l, 'r, L: ToType + $bound<R>, R: ToType + ToConstant> $trait<&Value<'r, R>>
            for Value<'l, L>
        {
            type Output = Value<'l, L::Result>;

            fn $fn_name(self, rhs: &Value<'r, R>) -> Self::Output {
                &self $op rhs
            }
        }

        impl<'l,'r, L: ToType + $bound<R>, R: ToType + ToConstant> $trait<Value<'r, R>>
            for Value<'l, L>
        {
            type Output = Value<'l, L::Result>;

            fn $fn_name(self, rhs: Value<'r, R>) -> Self::Output {
                &self $op &rhs
            }
        }

        impl<'a, L: ToType + $bound<R>, R: ToType + ToConstant> $trait<R> for Value<'a, L> {
            type Output = Value<'a, L::Result>;

            fn $fn_name(self, rhs: R) -> Self::Output {
                &self $op &self.fn_cx.const_(rhs)
            }
        }

        impl<'a, L: ToType + $bound<R>, R: ToType + ToConstant> $trait<R> for &Value<'a, L> {
            type Output = Value<'a, L::Result>;

            fn $fn_name(self, rhs: R) -> Self::Output {
                self $op &self.fn_cx.const_(rhs)
            }
        }
    };
}

binop!(Add when ValueMathOp: Add, add, +);
binop!(Sub when ValueMathOp: Subtract, sub, -);
binop!(Rem when ValueMathOp: Modulo, rem, %);
binop!(Mul when ValueMathOp: Multiply, mul, *);
binop!(Div when ValueMathOp: Divide, div, /);

impl<'a, R: ValueNegate + ToType> Neg for Value<'a, R> {
    type Output = Value<'a, R>;

    fn neg(self) -> Self::Output {
        self.with_expression(Expression::Unary {
            op: naga::UnaryOperator::Negate,
            expr: self.expr(),
        })
    }
}

impl<'a> Value<'a, u32> {
    /// A minimal version of saturating mul
    pub fn saturating_mul(
        &self,
        rhs: u32,
        cx: &mut BlockContext,
        result: Value<'_, impl PointerType<Pointee = u32>>,
        overflowed: Option<impl FnOnce(&mut BlockContext)>,
    ) {
        cx.if_(
            self.gt(u32::MAX / rhs),
            |cx| {
                cx.store(result, u32::MAX);
                if let Some(overflowed) = overflowed {
                    overflowed(cx);
                }
            },
            |cx| cx.store(result, self * rhs),
        );
    }
}
