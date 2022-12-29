use naga::{Handle, Module, ScalarKind, StructMember, Type, TypeInner};
use std::{
    any::TypeId,
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::SPAN;

pub trait ToType: 'static + Sized {
    fn naga_ty(registry: &mut TypeRegistry) -> Type {
        Type {
            name: Some(type_name_simple_of::<Self>()),
            inner: Self::naga_ty_inner(registry),
        }
    }
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner;
}

fn type_name_simple_of<T: 'static>() -> String {
    let name = std::any::type_name::<T>();
    let name_final = name.rsplit_terminator("::").next().unwrap();
    name_final.replace("<", "_").replace(">", "_")
}

impl ToType for u32 {
    fn naga_ty_inner(_: &mut TypeRegistry) -> TypeInner {
        naga::TypeInner::Scalar {
            kind: ScalarKind::Uint,
            width: 4,
        }
    }
}
impl ToType for i32 {
    fn naga_ty_inner(_: &mut TypeRegistry) -> TypeInner {
        naga::TypeInner::Scalar {
            kind: ScalarKind::Sint,
            width: 4,
        }
    }
}
impl ToType for f32 {
    fn naga_ty_inner(_: &mut TypeRegistry) -> TypeInner {
        naga::TypeInner::Scalar {
            kind: ScalarKind::Float,
            width: 4,
        }
    }
}
impl ToType for bool {
    fn naga_ty_inner(_: &mut TypeRegistry) -> TypeInner {
        naga::TypeInner::Scalar {
            kind: ScalarKind::Bool,
            width: naga::BOOL_WIDTH,
        }
    }
}

pub struct MyStruct {
    pub a: u32,
    pub b: f32,
}

impl ToType for MyStruct {
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner {
        let members = vec![
            StructMember {
                name: Some("a".to_string()),
                ty: registry.register_type::<u32>(),
                binding: None,
                offset: 0,
            },
            StructMember {
                name: Some("b".to_string()),
                ty: registry.register_type::<f32>(),
                binding: None,
                offset: 4,
            },
        ];
        TypeInner::Struct {
            members,
            span: std::mem::size_of::<Self>().try_into().unwrap(),
        }
    }
}

#[derive(Default)]
pub(crate) struct TypeMap {
    map: HashMap<TypeId, Handle<Type>>,
}

impl Deref for TypeMap {
    type Target = HashMap<TypeId, Handle<Type>>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl DerefMut for TypeMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}

pub struct TypeRegistry<'a> {
    tys: &'a mut TypeMap,
    module: &'a mut Module,
}

impl<'a> TypeRegistry<'a> {
    pub(crate) fn new(module: &'a mut Module, tys: &'a mut TypeMap) -> Self {
        Self { tys, module }
    }

    pub fn register_type<T: ToType>(&mut self) -> Handle<Type> {
        if let Some(ty) = self.tys.get(&TypeId::of::<T>()) {
            return ty.clone();
        }
        let ty_val = T::naga_ty(self);
        self.module.types.insert(ty_val, SPAN)
    }
}
