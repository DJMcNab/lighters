use naga::{
    AddressSpace, Constant, ConstantInner, Expression, Handle, Module, ScalarKind, ScalarValue,
    Type, TypeInner,
};
use std::{
    any::TypeId,
    collections::HashMap,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::{Value, SPAN};

pub trait ToType: 'static {
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner;

    fn naga_ty(registry: &mut TypeRegistry) -> Type {
        Type {
            name: Some(type_name_simple_of::<Self>()),
            inner: Self::naga_ty_inner(registry),
        }
    }

    fn type_handle(registry: &mut TypeRegistry) -> Handle<Type> {
        if let Some(ty) = registry.tys.get(&TypeId::of::<Self>()) {
            return *ty;
        }
        let ty_val = Self::naga_ty(registry);
        let handle = registry.module.types.insert(ty_val, SPAN);
        registry.tys.insert(TypeId::of::<Self>(), handle);
        handle
    }
}
pub trait ToConstant: ToType {
    fn naga_constant(&self, registry: &mut TypeRegistry) -> ConstantInner;
}

pub trait Vector: ToType {
    type Inner: ToType;
    fn len() -> u32;
}

pub trait PointerType: 'static + ToType {
    type Pointee: ToType;
    type This<U: ToType>: PointerType<Pointee = U>;
}

pub struct LocalVariable<T: ToType>(PhantomData<T>);
pub struct StoragePtr<T: ToType, A: StorageAccess>(PhantomData<fn() -> (Box<T>, A)>);
pub struct WorkgroupPtr<T: ToType>(PhantomData<T>);

pub trait StorageAccess: 'static {
    fn access() -> naga::StorageAccess;
}

pub struct ReadOnly;
pub struct WriteOnly;
pub struct ReadWrite;

impl StorageAccess for ReadOnly {
    fn access() -> naga::StorageAccess {
        naga::StorageAccess::LOAD
    }
}
impl StorageAccess for WriteOnly {
    fn access() -> naga::StorageAccess {
        naga::StorageAccess::STORE
    }
}
impl StorageAccess for ReadWrite {
    fn access() -> naga::StorageAccess {
        naga::StorageAccess::all()
    }
}

impl<T: ToType> PointerType for LocalVariable<T> {
    type Pointee = T;
    type This<U: ToType> = LocalVariable<U>;
}

impl<T: ToType> ToType for LocalVariable<T> {
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner {
        TypeInner::Pointer {
            base: registry.register_type::<T>(),
            space: AddressSpace::Function,
        }
    }
}

impl<T: ToType, A: StorageAccess> PointerType for StoragePtr<T, A> {
    type Pointee = T;
    type This<U: ToType> = StoragePtr<U, A>;
}

impl<T: ToType, A: StorageAccess> ToType for StoragePtr<T, A> {
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner {
        TypeInner::Pointer {
            base: registry.register_type::<T>(),
            space: AddressSpace::Storage {
                access: A::access(),
            },
        }
    }
}

impl<'a, P: PointerType> Value<'a, P> {
    pub fn load(&self) -> Value<'a, P::Pointee> {
        self.with_expression(Expression::Load {
            pointer: self.expr(),
        })
    }
}

impl<T: ToType> PointerType for WorkgroupPtr<T> {
    type Pointee = T;
    type This<U: ToType> = WorkgroupPtr<U>;
}

impl<T: ToType> ToType for WorkgroupPtr<T> {
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner {
        TypeInner::Pointer {
            base: registry.register_type::<T>(),
            space: AddressSpace::WorkGroup,
        }
    }
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
    ($type: ty: $len: literal, $inner_ty: ty, $size: ident, $kind: ident, $width: expr, ($($components: ident),+)) => {
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
        impl Vector for $type {
            type Inner = $inner_ty;
            fn len()->u32{
                $len
            }
        }
    };
}

impl<'a, V: Vector> Value<'a, V> {
    pub fn get_component(&self, index: u32) -> Value<'a, V::Inner> {
        self.with_expression(Expression::AccessIndex {
            base: self.expr(),
            index,
        })
    }
}

vector!(glam::u32::UVec2: 2, u32, Bi, Uint, 4, (x, y));
vector!(glam::u32::UVec3: 3, u32, Tri, Uint, 4, (x, y, z));
vector!(glam::u32::UVec4: 4, u32, Quad, Uint, 4, (x, y, z, w));

vector!(glam::i32::IVec2: 2, i32, Bi, Sint, 4, (x, y));
vector!(glam::i32::IVec3: 3, i32, Tri, Sint, 4, (x, y, z));
vector!(glam::i32::IVec4: 4, i32, Quad, Sint, 4, (x, y, z, w));

vector!(glam::f32::Vec2:2, f32,  Bi, Float, 4, (x, y));
vector!(glam::f32::Vec3:3, f32,  Tri, Float, 4, (x, y, z));
vector!(glam::f32::Vec4:4, f32,  Quad, Float, 4, (x, y, z, w));

vector!(glam::bool::BVec2: 2, bool, Bi, Bool, naga::BOOL_WIDTH, (x, y));
vector!(glam::bool::BVec3: 3, bool, Tri, Bool, naga::BOOL_WIDTH, (x, y, z));
vector!(
    glam::bool::BVec4: 4, bool, Quad,
    Bool,
    naga::BOOL_WIDTH,
    (x, y, z, w)
);

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

// Box is ued here to allow Sized
impl<T: ToType> ToType for Box<[T]> {
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner {
        let base = registry.register_type::<T>();
        let el_size = registry
            .module
            .types
            .get_handle(base)
            .unwrap()
            .inner
            .size(&registry.module.constants);
        TypeInner::Array {
            base,
            size: naga::ArraySize::Dynamic,
            // TODO: This is slightly wrong for different alignments
            // Good enough for testing
            stride: el_size,
        }
    }
}

impl<'a, T: ToType, P: PointerType<Pointee = Box<[T]>>> Value<'a, P> {
    pub fn index(&self, idx: Value<'_, u32>) -> Value<'a, P::This<T>> {
        self.with_expression(Expression::Access {
            base: self.expr(),
            index: idx.expr(),
        })
    }
}

pub struct Array<T: ToType, const N: u32>(PhantomData<T>);

impl<'a, T: ToType, const N: u32> ToType for Array<T, N> {
    fn naga_ty_inner(registry: &mut TypeRegistry) -> TypeInner {
        let base = registry.register_type::<T>();
        let el_size = registry
            .module
            .types
            .get_handle(base)
            .unwrap()
            .inner
            .size(&registry.module.constants);
        TypeInner::Array {
            base,
            size: naga::ArraySize::Constant(registry.register_constant(N)),
            // TODO: This is slightly wrong for different alignments
            // Good enough for testing
            stride: el_size,
        }
    }
}

impl<'a, T: ToType, const N: u32, P: PointerType<Pointee = Array<T, N>>> Value<'a, P> {
    pub fn get_index(&self, idx: &Value<'_, u32>) -> Value<'a, P::This<T>> {
        self.with_expression(Expression::Access {
            base: self.expr(),
            index: idx.expr(),
        })
    }
}

fn type_name_simple_of<T: 'static + ?Sized>() -> String {
    let name = std::any::type_name::<T>();
    let name_final = name.rsplit_terminator("::").next().unwrap();
    name_final.replace(['<', '>'], "_")
}
