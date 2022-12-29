pub mod arguments;
pub mod types;

use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

use glam::UVec2;
use naga::{Block, Constant, Handle};
use naga::{EntryPoint, Expression, Function, Module, Span, Statement, Type};

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

// macro_rules! Let {
//     ($name: ident = $val: expr) => {
//         let $name = ($val).named(stringify!($name));
//     };
// }

pub struct ModuleContext {
    module: Module,
    type_map: TypeMap,
}

impl Default for ModuleContext {
    fn default() -> Self {
        Self {
            module: Default::default(),
            type_map: Default::default(),
        }
    }
}

impl ModuleContext {
    fn type_for<T: ToType>(&mut self) -> Handle<Type> {
        TypeRegistry::new(&mut self.module, &mut self.type_map).register_type::<T>()
    }
}

struct FunctionContext<'a> {
    module: &'a mut ModuleContext,
    function: &'a mut Function,
}

#[derive(Clone)]
struct FnCx<'a>(Rc<RefCell<FunctionContext<'a>>>);

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
        self.with_module_cx(|module| {
            let mut registry = TypeRegistry::new(&mut module.module, &mut module.type_map);
            registry.register_constant_named(val, name)
        })
    }
}

pub struct BlockContext<'a> {
    function: FnCx<'a>,
    block: Block,
}

pub struct Value<'a, T: ToType> {
    expr: Handle<Expression>,
    fn_cx: FnCx<'a>,
    val: PhantomData<fn() -> T>,
}

impl<'a, T: ToType> Value<'a, T> {
    fn with_expression<U: ToType>(&self, expression: Expression) -> Value<'a, U> {
        Value {
            expr: self.fn_cx.add_expression(expression),
            fn_cx: self.fn_cx.clone(),
            val: PhantomData,
        }
    }

    fn named(&self, name: impl Into<String>) -> Option<String> {
        self.fn_cx
            .with_function(|f| f.named_expressions.insert(self.expr, name.into()))
    }
}

impl<'a> Value<'a, u32> {
    fn add(&self, other: &Value<'a, u32>) -> Value<'a, u32> {
        self.with_expression(Expression::Binary {
            op: naga::BinaryOperator::Add,
            left: self.expr,
            right: other.expr,
        })
    }
}

pub use Value as V;

pub fn module() -> Module {
    let mut module_cx = ModuleContext::default();

    let u32_ty = module_cx.type_for::<u32>();

    let mut function = Function::default();

    let entry_point = {
        let context = FunctionContext {
            module: &mut module_cx,
            function: &mut function,
        };
        let fn_cx = FnCx(Rc::new(RefCell::new(context)));
        fn_cx.add_named_constant(UVec2::new(10, 20), "test");
        fn_cx.add_named_constant(UVec2::new(20, 30), "test23");
        EntryPoint {
            name: "main".to_string(),
            stage: naga::ShaderStage::Compute,
            early_depth_test: None,
            workgroup_size: [256, 1, 1],
            function,
        }
    };
    module_cx.module.entry_points.push(entry_point);
    module_cx.module
}
