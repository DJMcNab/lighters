use naga::{Function, Module, Span};

pub fn module() -> Module {
    let mut module = Module::default();
    let function = Function::default();
    module.functions.append(function, Span::new(0, 10));
    return module;
}
