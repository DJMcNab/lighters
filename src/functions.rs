use std::{
    any::TypeId,
    collections::HashMap,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use naga::{Expression, Function, Handle, Statement, Type};

use crate::{block::BlockContext, ToType, TypeRegistry, Value};

mod entry_point;

pub trait ShaderFunction<'a, Marker, Args>: Sized {
    type Return: FunctionReturn;
    fn argument_types(registry: &mut TypeRegistry) -> Vec<Handle<Type>>;

    fn call(self, block: &mut BlockContext);
}

pub struct FnItem;

pub trait FunctionReturn {
    fn return_type(registry: &mut TypeRegistry) -> Option<Handle<Type>>;
    fn return_statement(&self) -> Option<naga::Statement>;
}

impl<T: ToType> FunctionReturn for Returned<T> {
    fn return_type(registry: &mut TypeRegistry) -> Option<Handle<Type>> {
        Some(registry.register_type::<T>())
    }
    fn return_statement(&self) -> Option<Statement> {
        Some(Statement::Return {
            value: Some(self.expr),
        })
    }
}
impl FunctionReturn for () {
    fn return_type(_: &mut TypeRegistry) -> Option<Handle<Type>> {
        None
    }
    fn return_statement(&self) -> Option<naga::Statement> {
        None
    }
}

pub struct Returned<T: ToType> {
    expr: Handle<Expression>,
    val: PhantomData<T>,
}

impl<'a, T: ToType> Value<'a, T> {
    pub fn as_return(&self) -> Returned<T> {
        Returned {
            expr: self.expr(),
            val: PhantomData,
        }
    }
}

macro_rules! impl_function {
    ($($idents: ident),*) => {
        impl<'a, Fun, Ret: FunctionReturn, $($idents: ToType),*>
        ShaderFunction<'a, (FnItem, Ret), ($(Value<'a, $idents>,)*)> for Fun where Fun: for <'b, 'c> FnOnce(&'b mut BlockContext<'c>, $(Value<'c, $idents>),*) -> Ret {
            type Return = Ret;
            fn argument_types(_registry: &mut TypeRegistry) -> Vec<Handle<Type>> {
                vec![
                    $(_registry.register_type::<$idents>()),*
                ]
            }

            #[allow(unused_assignments, non_snake_case)]
            fn call(self, block: &mut BlockContext) {
                #[allow(unused)]
                let mut count = 0;
                $(
                    let $idents = Value::new(Expression::FunctionArgument(count), &block);
                    count += 1;
                )*
                let ret = self(block, $($idents),*);
                if let Some(stmt) = ret.return_statement() {
                    block.add_statement(stmt);
                }
            }
        }
    };
}

impl_function!();
impl_function!(A);
impl_function!(A, B);
impl_function!(A, B, C);
impl_function!(A, B, C, D);
impl_function!(A, B, C, E, F);
impl_function!(A, B, C, E, F, G);
impl_function!(A, B, C, E, F, G, H);
impl_function!(A, B, C, E, F, G, H, I);
impl_function!(A, B, C, E, F, G, H, I, J);
impl_function!(A, B, C, E, F, G, H, I, J, K);
impl_function!(A, B, C, E, F, G, H, I, J, K, L);
impl_function!(A, B, C, E, F, G, H, I, J, K, L, M);

#[derive(Default)]
pub(crate) struct FunctionMap {
    map: HashMap<TypeId, Handle<Function>>,
}

impl Deref for FunctionMap {
    type Target = HashMap<TypeId, Handle<Function>>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl DerefMut for FunctionMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}
