//! Adapted from https://github.com/gfx-rs/naga/blob/224ff3897d51aa5bd85d296d0e41171dfa7a5b35/src/front/mod.rs,
//! used under the MIT license, which is provided at the bottom of this file.
//!
//! Changes are made to allow including multiple expression ranges
use naga::{Arena, Expression, Statement};

/// Helper class to emit expressions
#[derive(Default, Debug)]
pub(super) struct Emitter {
    past_statements: Vec<Statement>,
    start_len: Option<usize>,
}

impl Emitter {
    pub fn start(&mut self, arena: &Arena<Expression>) {
        if self.start_len.is_some() {
            unreachable!("Emitting has already started!");
        }
        self.start_len = Some(arena.len());
    }

    #[must_use]
    fn finish_single(&mut self, arena: &Arena<Expression>) -> Option<Statement> {
        let start_len = self.start_len.take().unwrap();
        if start_len != arena.len() {
            let range = arena.range_from(start_len);
            Some(Statement::Emit(range))
        } else {
            None
        }
    }

    pub fn pause(&mut self, arena: &Arena<Expression>) {
        let new_statement = self.finish_single(arena);
        self.past_statements.extend(new_statement);
    }

    pub fn finish<'a>(
        &'a mut self,
        arena: &Arena<Expression>,
    ) -> impl Iterator<Item = Statement> + 'a {
        self.pause(arena);
        self.past_statements.drain(..)
    }
}

// Copyright (c) [yyyy] [name of copyright owner]
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
