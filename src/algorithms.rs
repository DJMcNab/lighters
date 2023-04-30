use std::ops::{Add, Mul};

use crate::{
    entry_point::LocalInvocationId,
    types::{Array, ToConstant, Vector, WorkgroupPtr},
    BlockContext, Returned, ToType, Value,
};

pub fn scan<T: ToType + ToConstant, Op: ScanOp<T>, const WORKGROUP_WIDTH: u32>(
    cx: &mut BlockContext,
    local_id: Value<LocalInvocationId>,
    // TODO: This should always be a constant?
    // workgroup_size: Value<WorkgroupSize>,
    scratch: Value<WorkgroupPtr<Array<T, WORKGROUP_WIDTH>>>,
    initial: Value<T>,
) -> Returned<T> {
    let agg = cx.local_variable("agg");
    cx.store(&agg, &initial);
    let idx = local_id.inner().get_component(0);
    let this_val = scratch.get_index(&idx);
    cx.store(&this_val, &(agg.load()));
    for i in 0..WORKGROUP_WIDTH.ilog2() {
        cx.barrier();
        let new_index = &idx + (1 << i);
        cx.if_(
            &new_index.lt(&cx.const_(WORKGROUP_WIDTH)),
            |cx| {
                let other = scratch.get_index(&new_index);
                let result = Op::run(&agg.load(), &other.load());
                cx.store(&agg, &result);
            },
            |_| {},
        );
        cx.barrier();
        cx.store(&this_val, &agg.load());
    }

    agg.load().as_return()
}

pub trait ScanOp<T: ToType> {
    fn run<'l>(lhs: &Value<'l, T>, rhs: &Value<'_, T>) -> Value<'l, T>;
}
impl<T: ToType> ScanOp<T> for Sum
where
    for<'a, 'l, 'r> &'a Value<'l, T>: Add<&'a Value<'r, T>, Output = Value<'l, T>>,
{
    fn run<'l>(lhs: &Value<'l, T>, rhs: &Value<'_, T>) -> Value<'l, T> {
        lhs + rhs
    }
}

pub fn reduce_vecn<V: Vector, Op: ReduceOp<V::Inner>>(
    _: &mut BlockContext,
    v: Value<V>,
) -> Returned<V::Inner> {
    // result stores the current running total; this variable stores $Value$s
    let mut result = v.get_component(0);
    // Iterate through the remaining indices of the vector
    for i in 1..V::len() {
        result = Op::run(&result, &v.get_component(i));
    }
    result.as_return()
}

pub trait ReduceOp<T: ToType> {
    fn run<'a>(lhs: &Value<'a, T>, rhs: &Value<'a, T>) -> Value<'a, T>;
}

pub struct Sum;

impl<T: ToType> ReduceOp<T> for Sum
where
    for<'a, 'l, 'r> &'l Value<'a, T>: Add<&'l Value<'a, T>, Output = Value<'a, T>>,
{
    fn run<'a, 'b>(lhs: &Value<'a, T>, rhs: &Value<'a, T>) -> Value<'a, T> {
        lhs + rhs
    }
}

pub struct Product;
impl<T: ToType> ReduceOp<T> for Product
where
    for<'a, 'l, 'r> &'l Value<'a, T>: Mul<&'l Value<'a, T>, Output = Value<'a, T>>,
{
    fn run<'a, 'b>(lhs: &Value<'a, T>, rhs: &Value<'a, T>) -> Value<'a, T> {
        lhs * rhs
    }
}
