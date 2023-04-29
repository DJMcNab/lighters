pub mod algorithms;
mod block;
mod emitter;
mod functions;
pub mod types;
pub mod value;

use std::any::TypeId;
use std::cell::RefCell;
use std::rc::Rc;

pub use block::BlockContext;
use emitter::Emitter;
use functions::{FunctionMap, FunctionReturn, ShaderFunction};
use glam::Vec4;
use naga::{Constant, FunctionArgument, FunctionResult, GlobalVariable, Handle, ResourceBinding};
use naga::{EntryPoint, Expression, Function, Module, Span};

pub use functions::Returned;
pub use naga;

/// We unfortunately need to use a null span for all operations, because naga only understands single-file
/// sources. In theory, we could use `Span` as a `u64` identifier, although since wgpu would panic if the error was printed
/// (e.g. with a naïve `unwrap`), that's not ideal
///
/// It may be fruitful to store our own spans in a side-channel for our own error reporting if there is a naga validation failures, or
/// adapt wgpu to handle this case "gracefully"
///
/// It seems surprising that the spir-v or glsl backends don't run into this, although I haven't looked into it
const SPAN: Span = Span::UNDEFINED;

use types::{StorageAccess, StoragePtr, TypeMap};
use types::{ToConstant, WorkgroupPtr};

pub use types::{ToType, TypeRegistry};
pub use value::{entry_point, Value};

use crate::algorithms::{reduce_vecn, Sum};

macro_rules! Let {
    ($name: ident = $val: expr) => {
        statement!(_fake, let $name = $val);
    };
}

#[derive(Default)]
pub struct ModuleContext {
    module: Module,
    type_map: TypeMap,
    functions: FunctionMap,
}

impl ModuleContext {
    fn registry(&mut self) -> TypeRegistry {
        TypeRegistry::new(&mut self.module, &mut self.type_map)
    }

    pub fn module(self) -> Module {
        self.module
    }
}

struct FunctionContext<'a> {
    module: &'a mut ModuleContext,
    function: &'a mut Function,
    emitter: Emitter,
}

pub struct EntryPointContext<'a> {
    cx: FnCx<'a>,
    has_body: bool,
    workgroup_size: [u32; 3],
    workgroup_size_expr: Option<Value<'a, entry_point::WorkgroupSize>>,
}

impl<'a> EntryPointContext<'a> {
    pub fn body(&mut self, f: impl FnOnce(&mut BlockContext<'a>)) {
        assert!(!self.has_body, "Can only add one body to an entry point");
        self.has_body = true;

        let mut block_ctx = BlockContext::new_unstarted(self.cx.clone());
        f(&mut block_ctx);
        block_ctx.emit();
        self.cx.with_function(|f| f.body = block_ctx.block);
    }

    pub fn global_invocation_id(&mut self) -> Value<'a, entry_point::GlobalInvocationId> {
        self.binding_argument(
            naga::Binding::BuiltIn(naga::BuiltIn::GlobalInvocationId),
            "global_invocation_id",
        )
    }
    pub fn local_invocation_id(&mut self) -> Value<'a, entry_point::LocalInvocationId> {
        self.binding_argument(
            naga::Binding::BuiltIn(naga::BuiltIn::LocalInvocationId),
            "local_invocation_id",
        )
    }
    pub fn local_invocation_index(&mut self) -> Value<'a, entry_point::LocalInvocationIndex> {
        self.binding_argument(
            naga::Binding::BuiltIn(naga::BuiltIn::LocalInvocationIndex),
            "local_invocation_index",
        )
    }
    pub fn work_group_id(&mut self) -> Value<'a, entry_point::WorkGroupId> {
        self.binding_argument(
            naga::Binding::BuiltIn(naga::BuiltIn::WorkGroupId),
            "work_group_id",
        )
    }
    pub fn num_work_groups(&mut self) -> Value<'a, entry_point::NumWorkGroups> {
        self.binding_argument(
            naga::Binding::BuiltIn(naga::BuiltIn::NumWorkGroups),
            "num_work_groups",
        )
    }

    pub fn workgroup_size(&mut self) -> Value<'a, entry_point::WorkgroupSize> {
        if let Some(size) = &self.workgroup_size_expr {
            return size.clone();
        }
        let size_constant = self
            .cx
            .add_named_constant(glam::UVec3::from(self.workgroup_size), "WORKGROUP_SIZE");
        let value = Value::from_const_handle(size_constant, &self.cx);
        self.workgroup_size_expr = Some(value.clone());
        value
    }

    fn binding_argument<T: ToType>(
        &mut self,
        binding: naga::Binding,
        param_name: &'static str,
    ) -> Value<'a, T> {
        let ty = self.cx.with_module_cx(|module| {
            let mut registry = module.registry();
            registry.register_type::<T>()
        });
        let index = self.cx.with_function(|f| {
            let index = f.arguments.len();
            f.arguments.push(FunctionArgument {
                ty,
                name: Some(param_name.to_string()),
                binding: Some(binding),
            });
            index as u32
        });
        Value::new(Expression::FunctionArgument(index), &self.cx)
    }

    pub fn storage_variable<T: ToType, A: StorageAccess>(
        &mut self,
        name: &str,
        binding: ResourceBinding,
    ) -> Value<'a, StoragePtr<T, A>> {
        let handle = self.cx.with_full_context(|cx| {
            let ty = cx.module.registry().register_type::<T>();
            cx.module.module.global_variables.append(
                GlobalVariable {
                    name: Some(name.to_string()),
                    space: naga::AddressSpace::Storage {
                        access: A::access(),
                    },
                    binding: Some(binding),
                    ty,
                    init: None,
                },
                SPAN,
            )
        });
        Value::new(Expression::GlobalVariable(handle), &self.cx)
    }

    pub fn workgroup_variable<T: ToType>(&mut self, name: &str) -> Value<'a, WorkgroupPtr<T>> {
        let handle = self.cx.with_full_context(|cx| {
            let ty = cx.module.registry().register_type::<T>();
            cx.module.module.global_variables.append(
                GlobalVariable {
                    name: Some(name.to_string()),
                    space: naga::AddressSpace::WorkGroup,
                    binding: None,
                    ty,
                    init: None,
                },
                SPAN,
            )
        });
        Value::new(Expression::GlobalVariable(handle), &self.cx)
    }
}

impl ModuleContext {
    fn add_function<'a, F: ShaderFunction<'a, M, A> + 'static, M, A>(
        &mut self,
        f: F,
    ) -> Handle<Function> {
        if let Some(existing) = self.functions.get(&TypeId::of::<F>()) {
            return *existing;
        }
        let mut function = Function::default();
        let mut registry = self.registry();
        function.arguments = F::argument_types(&mut registry)
            .into_iter()
            .map(|ty| FunctionArgument {
                ty,
                name: None,
                binding: None,
            })
            .collect();
        function.result = <F::Return as FunctionReturn>::return_type(&mut registry)
            .map(|ty| FunctionResult { ty, binding: None });
        function.name = Some(std::any::type_name::<F>().to_string());
        {
            let fn_cx = FnCx::new(FunctionContext {
                module: &mut *self,
                function: &mut function,
                emitter: Default::default(),
            });
            let mut block_ctx = BlockContext::new(fn_cx);
            f.call(&mut block_ctx);
            block_ctx.emit();
            function.body = block_ctx.block;
        }
        let func = self.module.functions.append(function, SPAN);
        self.functions.insert(TypeId::of::<F>(), func);
        func
    }

    #[track_caller]
    pub fn entry_point(
        &mut self,
        name: impl Into<String>,
        workgroup_size: [u32; 3],
        f: impl FnOnce(&mut EntryPointContext),
    ) {
        let mut function = Function::default();

        {
            let fn_cx = FnCx::new(FunctionContext {
                module: &mut *self,
                function: &mut function,
                emitter: Default::default(),
            });
            fn_cx.with_full_context(|e| e.emitter.start(&e.function.expressions));
            let mut ep_cx = EntryPointContext {
                cx: fn_cx,
                has_body: false,
                workgroup_size,
                workgroup_size_expr: None,
            };
            f(&mut ep_cx);
            if !ep_cx.has_body {
                panic!("Must add a body to an entry point. Call `body` on the provided EntryPointContext");
            }
        }

        self.module.entry_points.push(EntryPoint {
            name: name.into(),
            stage: naga::ShaderStage::Compute,
            early_depth_test: None,
            workgroup_size,
            function,
        })
    }
}

#[derive(Clone)]
pub struct FnCx<'a>(Rc<RefCell<FunctionContext<'a>>>);

impl<'a> FnCx<'a> {
    /// Creates a new [`FnCx`].
    fn new(fun: FunctionContext<'a>) -> Self {
        Self(Rc::new(RefCell::new(fun)))
    }
}

impl<'a> FnCx<'a> {
    fn with_full_context<R>(&self, f: impl FnOnce(&mut FunctionContext) -> R) -> R {
        f(&mut self.0.borrow_mut())
    }

    pub fn with_function<R>(&self, f: impl FnOnce(&mut Function) -> R) -> R {
        self.with_full_context(|ctx| f(ctx.function))
    }

    pub fn with_module_cx<R>(&self, f: impl FnOnce(&mut ModuleContext) -> R) -> R {
        self.with_full_context(|ctx| f(ctx.module))
    }

    pub fn add_expression(&self, expression: Expression) -> Handle<Expression> {
        self.with_full_context(|ctx| {
            let skip_emit = no_emit(&expression);
            if skip_emit {
                ctx.emitter.pause(&ctx.function.expressions);
            }
            let handle = ctx.function.expressions.append(expression, SPAN);
            if skip_emit {
                ctx.emitter.start(&ctx.function.expressions);
            }
            handle
        })
    }

    pub fn add_constant<T: ToConstant>(&self, val: T) -> Handle<Constant> {
        self.with_module_cx(|module| {
            let mut registry = TypeRegistry::new(&mut module.module, &mut module.type_map);
            registry.register_constant(val)
        })
    }

    pub fn add_named_constant<T: ToConstant>(
        &self,
        val: T,
        name: impl Into<String>,
    ) -> Handle<Constant> {
        self.with_module_cx(|module| module.registry().register_constant_named(val, name))
    }
    pub fn const_<T: ToConstant + ToType>(&self, val: T) -> value::Value<'a, T> {
        let constant = self.add_constant(val);

        Value::new(Expression::Constant(constant), self)
    }
}

/// Expressions which do not need a [`naga::Statement::Emit`]
fn no_emit(expression: &Expression) -> bool {
    match expression {
        Expression::Constant(_)
        | Expression::FunctionArgument(_)
        | Expression::LocalVariable(_)
        | Expression::GlobalVariable(_)
        | Expression::CallResult(_)
        | Expression::AtomicResult { .. } => true,

        _ => {
            debug_assert!(!expression.needs_pre_emit());
            false
        }
    }
}

pub fn module() -> Module {
    let mut module_cx = ModuleContext::default();

    module_cx.entry_point("main", [256, 1, 1], |ep| {
        let workgroup_size = ep.workgroup_size();
        let local_id = ep.local_invocation_id();
        ep.body(|cx| {
            Let!(x = workgroup_size);
            let vec = cx.const_(Vec4::new(20., 30., 14., 54.));
            cx.call_function(reduce_vecn::<_, Sum>, &(vec,));
            statement!(cx, identity(x));
            statement!(cx, identity(local_id));
            statement!(
                cx,
                if (cx.const_(true)) {
                } else {
                }
            )
        })
    });

    module_cx.module
}

fn identity<T: ToType>(_cx: &mut BlockContext, val: Value<T>) -> Returned<T> {
    val.as_return()
}
