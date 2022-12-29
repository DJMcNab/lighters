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

macro_rules! vector {
    ($type: ty: $size: ident, $kind: ident, $width: expr, ($($components: ident),+)) => {
        impl ToType for $type {
            fn naga_ty_inner(_: &mut TypeRegistry) -> TypeInner {
                TypeInner::Vector {
                    size: naga::VectorSize::$size,
                    kind: ScalarKind::$kind,
                    width: $width,
                }
            }
        }

        impl ToConstant for $type {
            fn naga_constant(&self, registry: &mut TypeRegistry) -> ConstantInner {
                ConstantInner::Composite {
                    ty: registry.register_type::<Self>(),
                    components: vec![
                        $(registry.register_constant(self.$components)),+
                    ],
                }
            }
        }
    };
}

vector!(glam::u32::UVec2: Bi, Uint, 4, (x, y));
vector!(glam::u32::UVec3: Tri, Uint, 4, (x, y, z));
vector!(glam::u32::UVec4: Quad, Uint, 4, (x, y, z, w));

vector!(glam::i32::IVec2: Bi, Sint, 4, (x, y));
vector!(glam::i32::IVec3: Tri, Sint, 4, (x, y, z));
vector!(glam::i32::IVec4: Quad, Sint, 4, (x, y, z, w));

vector!(glam::f32::Vec2: Bi, Float, 4, (x, y));
vector!(glam::f32::Vec3: Tri, Float, 4, (x, y, z));
vector!(glam::f32::Vec4: Quad, Float, 4, (x, y, z, w));

vector!(glam::bool::BVec2: Bi, Bool, naga::BOOL_WIDTH, (x, y));
vector!(glam::bool::BVec3: Tri, Bool, naga::BOOL_WIDTH, (x, y, z));
vector!(
    glam::bool::BVec4: Quad,
    Bool,
    naga::BOOL_WIDTH,
    (x, y, z, w)
);

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
