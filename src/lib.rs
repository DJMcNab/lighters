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
use functions::{FunctionMap, FunctionReturn, Returned, ShaderFunction};
use naga::{Constant, FunctionArgument, FunctionResult, Handle};
use naga::{EntryPoint, Expression, Function, Module, Span};

/// We unfortunately need to use a null span for all operations, because naga only understands single-file
/// sources. In theory, we could use `Span` as a `u64` identifier, although since wgpu would panic if the error was printed
/// (e.g. with a naïve `unwrap`), that's not ideal
///
/// It may be fruitful to store our own spans in a side-channel for our own error reporting if there is a naga validation failures, or
/// adapt wgpu to handle this case "gracefully"
///
/// It seems surprising that the spir-v or glsl backends don't run into this, although I haven't looked into it
const SPAN: Span = Span::UNDEFINED;

use types::ToConstant;
use types::TypeMap;

pub use types::{ToType, TypeRegistry};
pub use value::Value;

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
}

struct FunctionContext<'a> {
    module: &'a mut ModuleContext,
    function: &'a mut Function,
    emitter: Emitter,
}

struct EntryPointContext<'a> {
    module: &'a mut ModuleContext,
    function: &'a mut Function,
}

impl<'a> EntryPointContext<'a> {
    fn body(&mut self, f: impl FnOnce(&mut BlockContext)) {
        let fn_cx = FnCx::new(FunctionContext {
            module: &mut self.module,
            function: &mut self.function,
            emitter: Emitter::default(),
        });
        let mut block_ctx = BlockContext::new(fn_cx);
        f(&mut block_ctx);
        block_ctx.emit();
        self.function.body = block_ctx.block;
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

    fn entry_point(
        &mut self,
        name: impl Into<String>,
        workgroup_size: [u32; 3],
        f: impl FnOnce(&mut BlockContext),
    ) {
        let mut function = Function::default();

        {
            let fn_cx = FnCx::new(FunctionContext {
                module: self,
                function: &mut function,
                emitter: Default::default(),
            });
            let mut block_ctx = BlockContext::new(fn_cx);
            f(&mut block_ctx);
            function.body = block_ctx.block;
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
        self.with_full_context(|ctx| f(&mut ctx.function))
    }

    pub fn with_module_cx<R>(&self, f: impl FnOnce(&mut ModuleContext) -> R) -> R {
        self.with_full_context(|ctx| f(&mut ctx.module))
    }

    pub fn add_expression(&self, expression: Expression) -> Handle<Expression> {
        self.with_full_context(|ctx| {
            let needs_no_emit = expression.needs_pre_emit();
            if needs_no_emit {
                ctx.emitter.pause(&ctx.function.expressions);
            }
            let handle = ctx.function.expressions.append(expression, SPAN);
            if needs_no_emit {
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

        Value::new(Expression::Constant(constant), &self)
    }
}

pub fn module() -> Module {
    let mut module_cx = ModuleContext::default();

    module_cx.add_function(|cx: &mut BlockContext| -> Returned<u32> {
        let x = statement!(cx, test_fn(cx.const_(1), cx.const_(3)));
        statement!(
            cx,
            if (cx.const_(true)) {
                statement!(cx, return x);
            }
        );
        x.as_return()
    });

    module_cx.module
}

fn test_fn<'a>(_cx: &mut BlockContext<'a>, a: Value<'a, u32>, b: Value<'a, u32>) -> Returned<u32> {
    (a + b).as_return()
}
