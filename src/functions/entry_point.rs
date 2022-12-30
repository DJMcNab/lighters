use crate::{ToType, Value};

pub mod compute {
    use glam::UVec3;

    use super::EntryPointArgument;

    macro_rules! builtin_binding {
        ($(#[$attrs: meta])* struct $type: ident: $inner: ty) => {
            $(#[$attrs])*
            pub struct $type;
            impl EntryPointArgument for $type {
                type Inner = $inner;

                fn binding() -> naga::Binding {
                    naga::Binding::BuiltIn(naga::BuiltIn::$type)
                }
            }
        };
    }
    builtin_binding!(struct GlobalInvocationId: UVec3);
    builtin_binding!(struct LocalInvocationId: UVec3);

    builtin_binding!(struct LocalInvocationIndex: u32);
    builtin_binding!(struct WorkGroupId: UVec3);
    // builtin_binding!(
    //     /// See https://github.com/gpuweb/gpuweb/issues/1590
    //     /// However, because this is algorithmically useful, we add our own support
    //     /// for it. We define it to be a constant of the provided workgroup size
    //     struct WorkGroupSize: UVec3);
    builtin_binding!(struct NumWorkGroups: UVec3);
}

pub trait EntryPointArgument: 'static + Sized {
    type Inner: ToType;

    fn binding() -> naga::Binding;
}

impl<T: EntryPointArgument> ToType for T {
    fn naga_ty_inner(_: &mut crate::TypeRegistry) -> naga::TypeInner {
        unimplemented!();
    }

    fn type_handle(registry: &mut crate::TypeRegistry) -> naga::Handle<naga::Type> {
        registry.register_type::<<Self as EntryPointArgument>::Inner>()
    }
}

impl<'a, T: EntryPointArgument> Value<'a, T> {
    pub fn inner(&self) -> Value<'a, T::Inner> {
        self.with_type()
    }
}
