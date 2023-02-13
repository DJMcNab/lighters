use std::ops::Deref;

use crate::{
    functions::{FunctionReturn, Returned, ShaderFunction},
    FnCx, ToType, Value, SPAN,
};
use naga::{Block, Statement};

pub struct BlockContext<'a> {
    function: FnCx<'a>,
    pub(crate) block: Block,
    has_returned: bool,
}

impl<'a> BlockContext<'a> {
    pub(crate) fn new(ctx: FnCx<'a>) -> BlockContext<'a> {
        let mut res = Self::new_unstarted(ctx);
        res.start();
        res
    }

    /// Used when creating entry points, as argument expressions are created
    /// before the block is created
    pub(crate) fn new_unstarted(ctx: FnCx<'a>) -> BlockContext<'a> {
        BlockContext {
            function: ctx,
            block: Default::default(),
            has_returned: false,
        }
    }

    fn start(&mut self) {
        self.function
            .with_full_context(|ctx| ctx.emitter.start(&ctx.function.expressions));
    }
    pub(crate) fn emit(&mut self) {
        self.function.with_full_context(|ctx| {
            let statements = ctx.emitter.finish(&ctx.function.expressions);
            for statement in statements {
                self.block.push(statement, SPAN);
            }
        });
    }

    // Most statements need their arguments to be emitted *before* being called
    pub fn add_statement(&mut self, stmt: naga::Statement) {
        self.emit();
        self.start();
        self.block.push(stmt, SPAN);
    }

    pub fn call_function<F: ShaderFunction<'a, M, A> + 'static, M, A>(
        &mut self,
        f: F,
        args: &A,
    ) -> <F::Return as FunctionReturn>::RetVal<'a> {
        let function = self.with_module_cx(|module| module.add_function(f));
        let return_val = <F::Return as FunctionReturn>::return_value(self, function);
        // The return expression, however, does not need to be emitted, as it is instead evaluated 'when' the call expression occurs
        // Note that if the expression is emitted, this appears to be *fine*, although it does create a 'phony' expression
        let return_expr = <F::Return as FunctionReturn>::return_expression(&return_val);
        self.add_statement(Statement::Call {
            function,
            arguments: F::argument_expressions(args),
            result: return_expr,
        });
        return_val
    }

    pub fn if_(
        &mut self,
        condition: &Value<bool>,
        then: impl FnOnce(&mut BlockContext<'a>),
        else_: impl FnOnce(&mut BlockContext<'a>),
    ) {
        // Make sure expressions in this block are included
        self.emit();
        let mut then_block = BlockContext::new(self.function.clone());
        then(&mut then_block);
        then_block.emit();
        let mut else_block = BlockContext::new(self.function.clone());
        else_(&mut else_block);
        else_block.emit();
        self.start();
        self.add_statement(Statement::If {
            condition: condition.expr(),
            accept: then_block.block,
            reject: else_block.block,
        });
    }

    pub fn return_<T: ToType>(&mut self, val: &Value<'a, T>) -> Returned<T> {
        if self.has_returned {
            return val.as_return();
        }
        self.has_returned = true;
        self.add_statement(Statement::Return {
            value: Some(val.expr()),
        });
        val.as_return()
    }
}

// No need for DerefMut, since `FnCx` only has & methods anyway
impl<'a> Deref for BlockContext<'a> {
    type Target = FnCx<'a>;

    fn deref(&self) -> &Self::Target {
        &self.function
    }
}

#[macro_export]
macro_rules! statement {
    ($block: ident, if ($val: expr) {$($then: stmt);* $(;)?}) => {
        statement!($block, if ($val) {$($then);*} else {});
    };
    ($block: ident, if ($val: expr) {$($then: stmt);* $(;)?} else {$($else_block: stmt);* $(;)?}) => {
        $block.if_(
            &$val,
            #[allow(redundant_semicolons)]
            |#[allow(unused)] $block| {
                $($then;)*
            },
            #[allow(redundant_semicolons)]
            |#[allow(unused)] $block| {
                $($else_block;)*
            },
        )
    };
    ($block: ident, let $name: ident = $expr: expr) => {
        let $name = ($expr).named(stringify!($name));
    };
    ($block: ident, $func: ident($($args: expr),* $(,)?)) => {
        $block.call_function($func, &($($args,)*))
    };
    ($block: ident, return $expr: expr) => {
        return $block.return_(&$expr).into();
    };
}
