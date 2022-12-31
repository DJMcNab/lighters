use std::ops::Deref;

use self::emitter::Emitter;
use crate::{
    functions::{FunctionReturn, ShaderFunction},
    FnCx, Value, SPAN,
};
use naga::{Block, Statement};

mod emitter {
    use naga::Arena;

    // Taken from naga/src/front/lib.rs
    /// Helper class to emit expressions
    #[allow(dead_code)]
    #[derive(Default, Debug)]
    pub(super) struct Emitter {
        start_len: Option<usize>,
    }

    #[allow(dead_code)]
    impl Emitter {
        pub fn start(&mut self, arena: &Arena<crate::Expression>) {
            if self.start_len.is_some() {
                unreachable!("Emitting has already started!");
            }
            self.start_len = Some(arena.len());
        }
        #[must_use]
        #[track_caller]
        pub fn finish(&mut self, arena: &Arena<crate::Expression>) -> Option<crate::Statement> {
            let start_len = self.start_len.take().unwrap();
            if start_len != arena.len() {
                let range = arena.range_from(start_len);
                Some(crate::Statement::Emit(range))
            } else {
                None
            }
        }
    }
}

pub struct BlockContext<'a> {
    function: FnCx<'a>,
    pub(crate) block: Block,
    emitter: Emitter,
}

impl<'a> BlockContext<'a> {
    pub(crate) fn new(ctx: FnCx<'a>) -> BlockContext<'a> {
        let mut res = BlockContext {
            function: ctx,
            block: Default::default(),
            emitter: Default::default(),
        };
        res.start();
        res
    }

    fn start(&mut self) {
        self.function
            .with_function(|f| self.emitter.start(&f.expressions));
    }
    pub(crate) fn emit(&mut self) {
        self.function.with_function(|f| {
            let Some(statement) = self.emitter.finish(&f.expressions) else {return};
            self.block.push(statement, SPAN);
        });
    }

    fn add_statement_raw(&mut self, stmt: naga::Statement) {
        self.block.push(stmt, SPAN);
    }

    pub fn add_statement(&mut self, stmt: naga::Statement) {
        self.emit();
        self.add_statement_raw(stmt);
        self.start();
    }

    pub fn call_function<F: ShaderFunction<'a, M, A> + 'static, M, A>(
        &mut self,
        f: F,
        args: A,
    ) -> <F::Return as FunctionReturn>::RetVal<'a> {
        let function = self.with_module_cx(|module| module.add_function(f));
        let return_val = <F::Return as FunctionReturn>::return_value(&self, function);
        let return_expr = <F::Return as FunctionReturn>::return_expression(&return_val);
        self.add_statement_raw(Statement::Call {
            function,
            arguments: F::argument_expressions(args),
            result: return_expr,
        });
        self.emit();
        self.start();
        return_val
    }

    pub fn if_(
        &mut self,
        condition: Value<bool>,
        then: impl FnOnce(&mut BlockContext<'a>),
        else_: impl FnOnce(&mut BlockContext<'a>),
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
            condition: condition.expr(),
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

#[macro_export]
macro_rules! statement {
    ($block: ident, if ($val: expr) {$($then: stmt);* $(;)?}) => {
        statement!($block, if ($val) {$(then);*} else {});
    };
    ($block: ident, if ($val: expr) {$($then: stmt);* $(;)?} else {$($else_block: stmt);* $(;)?}) => {
        $block.if_(
            $val,
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
}
