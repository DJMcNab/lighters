use std::ops::Deref;

use self::emitter::Emitter;
use crate::{FnCx, Value, SPAN};
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
            println!("Started");
        }
        #[must_use]
        #[track_caller]
        pub fn finish(&mut self, arena: &Arena<crate::Expression>) -> Option<crate::Statement> {
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

    pub fn add_statement(&mut self, stmt: naga::Statement) {
        self.emit();
        self.block.push(stmt, SPAN);
        self.start();
    }

    pub fn if_(
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

// See https://github.com/rust-lang/rustfmt/blob/ee2bed96d60fd7e46b1fb868f6a8f27e3a8058d0/src/macros.rs for justification for use of the `,` here
#[macro_export]
macro_rules! statements {
    ($block: ident, if ($val: expr) {$($then: tt)*} $(, $($tt: tt)*)?) => {
        $block.if_(
            $val,
            |$block| {
                statements!($block, $($then)*)
            },
            |$block| {},
        );
        $(statements!($block, $($tt)*))?
    };
    ($block: ident, if ($val: expr) {$then: expr} else {$else_block: expr} $(,$($tt: tt)+)?) => {
        $block.if_(
            $val,
            |$block| {
                $then;
            },
            |$block| {
                $else_block;
            },
        );
        $(statements!($block, $($tt)+))?
    };
    ($block: ident, let $name: ident = $expr: expr $(, $($tt: tt)*)?) => {
        let $name = ($expr).named(stringify!($name));
        $(statements!($block, $($tt)+))?
    };
}
