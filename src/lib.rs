pub mod types;

use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

use naga::{Block, Handle};
use naga::{
    EntryPoint, Expression, Function, FunctionArgument, Module, Span, Statement, Type, TypeInner,
};

/// We unfortunately need to use a null span for all operations, because naga only understands single-file
/// sources. In theory, we could use `Span` as a `u64` identifier, although since wgpu would panic if the error was printed
/// (e.g. with a naïve `unwrap`), that's not ideal
///
/// It may be fruitful to store our own spans in a side-channel for our own error reporting if there is a naga validation failures, or
/// adapt wgpu to handle this case "gracefully"
///
/// It seems surprising that the spir-v or glsl backends don't run into this, although I haven't looked into it
const SPAN: Span = Span::UNDEFINED;

use types::MyStruct;
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

    pub fn add_expression(&self, expression: Expression) -> Handle<Expression> {
        self.with_function(|f| f.expressions.append(expression, SPAN))
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
    function.arguments.push(FunctionArgument {
        name: Some("oh_neat".to_string()),
        ty: u32_ty,
        binding: Some(naga::Binding::Location {
            location: 0,
            interpolation: None,
            sampling: None,
        }),
    });
    let arg_expr = function
        .expressions
        .append(Expression::FunctionArgument(0), SPAN);
    let entry_point = {
        let context = FunctionContext {
            module: &mut module_cx,
            function: &mut function,
        };
        let fn_cx = FnCx(Rc::new(RefCell::new(context)));
        let arg_expr_value = Value::<u32> {
            expr: arg_expr,
            fn_cx,
            val: PhantomData,
        };
        arg_expr_value.add(&arg_expr_value).named("val");
        function
            .body
            .push(Statement::Emit(function.expressions.range_from(0)), SPAN);
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
