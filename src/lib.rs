pub mod arguments;
mod block;
pub mod types;
pub mod value;

use std::cell::RefCell;
use std::rc::Rc;

use block::BlockContext;
use naga::{Constant, Handle, Statement};
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
    // functions: FunctionMap
}

impl ModuleContext {
    fn registry(&mut self) -> TypeRegistry {
        TypeRegistry::new(&mut self.module, &mut self.type_map)
    }
}

struct FunctionContext<'a> {
    module: &'a mut ModuleContext,
    function: &'a mut Function,
}

impl ModuleContext {
    fn function(&mut self, f: impl FnOnce(&mut BlockContext)) -> Handle<Function> {
        let mut function = Function::default();

        {
            let fn_cx = FnCx::new(FunctionContext {
                module: self,
                function: &mut function,
            });
            let mut block_ctx = BlockContext::new(fn_cx);
            f(&mut block_ctx);
            block_ctx.emit();
            function.body = block_ctx.block;
        }
        self.module.functions.append(function, SPAN)
    }
    fn entry_point(&mut self, f: impl FnOnce(&mut BlockContext)) {
        let mut function = Function::default();

        {
            let fn_cx = FnCx::new(FunctionContext {
                module: self,
                function: &mut function,
            });
            let mut block_ctx = BlockContext::new(fn_cx);
            f(&mut block_ctx);
            function.body = block_ctx.block;
        }

        self.module.entry_points.push(EntryPoint {
            name: "test".to_string(),
            stage: naga::ShaderStage::Compute,
            early_depth_test: None,
            workgroup_size: [256, 1, 1],
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
    pub fn with_function<R>(&self, f: impl FnOnce(&mut Function) -> R) -> R {
        f(&mut self.0.borrow_mut().function)
    }

    pub fn with_module_cx<R>(&self, f: impl FnOnce(&mut ModuleContext) -> R) -> R {
        f(&mut self.0.borrow_mut().module)
    }

    pub fn add_expression(&self, expression: Expression) -> Handle<Expression> {
        self.with_function(|f| f.expressions.append(expression, SPAN))
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
    use crate::statement as s;
    let mut module_cx = ModuleContext::default();

    module_cx.function(|cx| {
        Let!(_true_result = cx.const_(1u32) + cx.const_(2u32));
        s!(
            cx,
            if (cx.const_(true)) {
                Let!(_x = &_true_result / 4);
            } else {
                Let!(_false_result = &_true_result + 5);
            }
        );
        Let!(_true_result = cx.const_(1u32) % 2u32);
    });

    module_cx.module
}
