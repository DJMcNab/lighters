use crate::{block::BlockContext, ToType, Value};

mod entry_point;

trait Function<Marker, Args> {}

struct FnItem;

impl<F> Function<FnItem, ()> for F where F: FnOnce(BlockContext) {}

impl<F, A: ToType> Function<FnItem, (A,)> for F where F: FnOnce(BlockContext, Value<A>) {}
impl<F, A: ToType, B: ToType> Function<FnItem, (A, B)> for F where
    F: FnOnce(BlockContext, Value<A>, Value<B>)
{
}
impl<F, A: ToType, B: ToType, C: ToType> Function<FnItem, (A, B, C)> for F where
    F: FnOnce(BlockContext, Value<A>, Value<B>, Value<C>)
{
}
