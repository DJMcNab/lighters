pub mod arguments;
pub mod types;

use std::cell::RefCell;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;

use naga::{Arena, Block, Constant, Handle, Statement};
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

// Taken from naga/src/front/lib.rs
/// Helper class to emit expressions
#[allow(dead_code)]
#[derive(Default, Debug)]
struct Emitter {
    start_len: Option<usize>,
}

#[allow(dead_code)]
impl Emitter {
    fn start(&mut self, arena: &Arena<crate::Expression>) {
        if self.start_len.is_some() {
            unreachable!("Emitting has already started!");
        }
        self.start_len = Some(arena.len());
        println!("Started");
    }
    #[must_use]
    #[track_caller]
    fn finish(&mut self, arena: &Arena<crate::Expression>) -> Option<crate::Statement> {
        println!("Finished");
        let start_len = self.start_len.take().unwrap();
        if start_len != arena.len() {
            let range = arena.range_from(start_len);
            Some(crate::Statement::Emit(range))
        } else {
            None
        }
    }
}

macro_rules! Let {
    ($name: ident = $val: expr) => {
        let $name = ($val).named(stringify!($name));
    };
}

pub struct ModuleContext {
    module: Module,
    type_map: TypeMap,
    // functions: FunctionMap
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
    pub fn const_<T: ToConstant + ToType>(&self, val: T) -> Value<'a, T> {
        let constant = self.add_constant(val);

        Value {
            expr: self.add_expression(Expression::Constant(constant)),
            fn_cx: self.clone(),
            val: PhantomData,
        }
    }
}

pub struct BlockContext<'a> {
    function: FnCx<'a>,
    block: Block,
    emitter: Emitter,
}

impl<'a> BlockContext<'a> {
    fn new(ctx: FnCx<'a>) -> BlockContext<'a> {
        let mut res = BlockContext {
            function: ctx,
            block: Default::default(),
            emitter: Default::default(),
        };
        res.start();
        res
    }

    fn add_statement(&mut self, stmt: naga::Statement) {
        self.emit();
        self.block.push(stmt, SPAN);
        self.start();
    }

    fn start(&mut self) {
        self.function
            .with_function(|f| self.emitter.start(&f.expressions));
    }
    fn emit(&mut self) {
        self.function.with_function(|f| {
            let Some(statement) = self.emitter.finish(&f.expressions) else {return};
            self.block.push(statement, SPAN);
        });
    }

    fn if_(
        &mut self,
        condition: Value<bool>,
        then: impl FnOnce(&mut BlockContext),
        else_: impl FnOnce(&mut BlockContext),
    ) {
        self.emit();
        let mut then_block = BlockContext::new(self.function.clone());
        then(&mut then_block);
        then_block.emit();
        let mut else_block = BlockContext::new(self.function.clone());
        else_(&mut else_block);
        else_block.emit();
        self.start();
        self.add_statement(Statement::If {
            condition: condition.expr,
            accept: then_block.block,
            reject: else_block.block,
        });
    }
}

// No need for DerefMut, since `FnCx` only has & methods anyway
impl<'a> Deref for BlockContext<'a> {
    type Target = FnCx<'a>;

    fn deref(&self) -> &Self::Target {
        &self.function
    }
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

impl<'a> std::ops::Add for Value<'a, u32> {
    type Output = Value<'a, u32>;

    fn add(self, rhs: Self) -> Self::Output {
        self.with_expression(Expression::Binary {
            op: naga::BinaryOperator::Add,
            left: self.expr,
            right: rhs.expr,
        })
    }
}

pub use Value as V;

pub fn module() -> Module {
    let mut module_cx = ModuleContext::default();

    module_cx.function(|cx| {
        cx.if_(
            cx.const_(true),
            |cx| {
                Let!(_true_result = cx.const_(1u32) + cx.const_(2u32));
            },
            |cx| {
                Let!(_else_result = cx.const_(4u32) + cx.const_(3u32));
            },
        );
    });

    // module_cx.entry_point(|fn_cx| {});

    module_cx.module
}
