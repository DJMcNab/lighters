use std::ops::{Add, Div, Mul, Rem, Sub};

use glam::{IVec2, IVec3, IVec4, UVec2, UVec3, UVec4, Vec2, Vec3, Vec4};
use naga::Expression;

use crate::{types::ToConstant, ToType, Value};

// Can be added/minused/multiplied/divided
trait ValueBinOp<Rhs = Self> {}
trait ValueNegate {}

impl ValueBinOp for u32 {}
impl ValueBinOp for i32 {}
impl ValueBinOp for f32 {}

impl ValueNegate for i32 {}
impl ValueNegate for f32 {}

macro_rules! vector {
    ($kind: ty: $component: ty, negates ) => {
        impl ValueNegate for $kind {}
        vector!($kind: $component);
    };
    ($kind: ty: $component: ty ) => {
        impl ValueBinOp<$component> for $kind {}
        impl ValueBinOp<$kind> for $component {}
        impl ValueBinOp<$kind> for $kind {}
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
    ($trait: ident: $operation: ident, $fn_name: ident, $op: tt) => {
        // Combinations needed: l + r; &l + r; l + &r; &l + &r; l + const; &l + const

        // This is the base implementation, other implementations all call into this
        impl<'l,'r, L: ToType + ValueBinOp<R>, R: ToType + ToConstant> $trait<&Value<'r, R>>
            for &Value<'l, L>
        {
            type Output = Value<'l, L>;

            fn $fn_name(self, rhs: &Value<'r, R>) -> Self::Output {
                self.with_expression(Expression::Binary {
                    op: naga::BinaryOperator::$operation,
                    left: self.expr,
                    right: rhs.expr,
                })
            }
        }
        impl<'l, 'r, L: ToType + ValueBinOp<R>, R: ToType + ToConstant> $trait<Value<'r, R>>
            for &Value<'l, L>
        {
            type Output = Value<'l, L>;

            fn $fn_name(self, rhs: Value<'r, R>) -> Self::Output {
                self $op &rhs
            }
        }
        impl<'l, 'r, L: ToType + ValueBinOp<R>, R: ToType + ToConstant> $trait<&Value<'r, R>>
            for Value<'l, L>
        {
            type Output = Value<'l, L>;

            fn $fn_name(self, rhs: &Value<'r, R>) -> Self::Output {
                &self $op rhs
            }
        }

        impl<'l,'r, L: ToType + ValueBinOp<R>, R: ToType + ToConstant> $trait<Value<'r, R>>
            for Value<'l, L>
        {
            type Output = Value<'l, L>;

            fn $fn_name(self, rhs: Value<'r, R>) -> Self::Output {
                &self $op &rhs
            }
        }

        impl<'a, L: ToType + ValueBinOp<R>, R: ToType + ToConstant> $trait<R> for Value<'a, L> {
            type Output = Value<'a, L>;

            fn $fn_name(self, rhs: R) -> Self::Output {
                &self $op &self.fn_cx.const_(rhs)
            }
        }

        impl<'a, L: ToType + ValueBinOp<R>, R: ToType + ToConstant> $trait<R> for &Value<'a, L> {
            type Output = Value<'a, L>;

            fn $fn_name(self, rhs: R) -> Self::Output {
                self $op &self.fn_cx.const_(rhs)
            }
        }
    };
}

binop!(Add: Add, add, +);
binop!(Sub: Subtract, sub, -);
binop!(Rem: Modulo, rem, %);
binop!(Mul: Multiply, mul, *);
binop!(Div: Divide, div, /);
