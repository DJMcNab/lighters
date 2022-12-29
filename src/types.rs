use naga::{
    Constant, ConstantInner, Handle, Module, ScalarKind, ScalarValue, StructMember, Type, TypeInner,
};
use std::{
    any::TypeId,
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::SPAN;

pub trait ToType: 'static + Sized {
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner;

    fn naga_ty(registry: &mut TypeRegistry) -> Type {
        Type {
            name: Some(type_name_simple_of::<Self>()),
            inner: Self::naga_ty_inner(registry),
        }
    }

    fn type_handle(registry: &mut TypeRegistry) -> Handle<Type> {
        if let Some(ty) = registry.tys.get(&TypeId::of::<Self>()) {
            return ty.clone();
        }
        let ty_val = Self::naga_ty(registry);
        registry.module.types.insert(ty_val, SPAN)
    }
}

fn type_name_simple_of<T: 'static>() -> String {
    let name = std::any::type_name::<T>();
    let name_final = name.rsplit_terminator("::").next().unwrap();
    name_final.replace("<", "_").replace(">", "_")
}

pub trait ToConstant: ToType {
    fn naga_constant(&self, registry: &mut TypeRegistry) -> ConstantInner;
}

macro_rules! scalar {
    ($type: ty, $kind: ident, $width: expr) => {
        impl ToType for $type {
            fn naga_ty_inner(_: &mut TypeRegistry) -> TypeInner {
                naga::TypeInner::Scalar {
                    kind: ScalarKind::$kind,
                    width: $width,
                }
            }
        }
        impl ToConstant for $type {
            fn naga_constant(&self, _: &mut TypeRegistry) -> ConstantInner {
                ConstantInner::Scalar {
                    width: $width,
                    value: ScalarValue::$kind((*self).into()),
                }
            }
        }
    };
}

scalar!(u32, Uint, 4);
scalar!(i32, Sint, 4);
scalar!(f32, Float, 4);
scalar!(bool, Bool, naga::BOOL_WIDTH);

impl ToType for glam::u32::UVec2 {
    fn naga_ty_inner(_: &mut TypeRegistry) -> TypeInner {
        TypeInner::Vector {
            size: naga::VectorSize::Bi,
            kind: ScalarKind::Uint,
            width: 4,
        }
    }
}

impl ToConstant for glam::u32::UVec2 {
    fn naga_constant(&self, registry: &mut TypeRegistry) -> ConstantInner {
        ConstantInner::Composite {
            ty: registry.register_type::<Self>(),
            components: vec![
                registry.register_constant(self.x),
                registry.register_constant(self.y),
            ],
        }
    }
}

impl ToType for glam::u32::UVec3 {
    fn naga_ty_inner(_: &mut TypeRegistry) -> TypeInner {
        TypeInner::Vector {
            size: naga::VectorSize::Tri,
            kind: ScalarKind::Uint,
            width: 4,
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
        T::type_handle(self)
    }

    pub fn register_constant_named<T: ToConstant>(
        &mut self,
        val: T,
        name: impl Into<String>,
    ) -> Handle<naga::Constant> {
        let inner = val.naga_constant(self);
        self.register_constant_inner(inner, Some(name.into()))
    }

    pub fn register_constant<T: ToConstant>(&mut self, val: T) -> Handle<naga::Constant> {
        let inner = val.naga_constant(self);
        self.register_constant_inner(inner, None)
    }

    fn register_constant_inner(
        &mut self,
        inner: ConstantInner,
        name: Option<String>,
    ) -> Handle<naga::Constant> {
        let constant = Constant {
            name,
            specialization: None,
            inner,
        };
        self.module.constants.append(constant, SPAN)
    }
}
