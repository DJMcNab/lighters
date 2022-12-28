pub mod types;

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
pub use types::TypeMap;
pub use types::{ToType, TypeRegistry};

pub fn module() -> Module {
    let mut types = TypeMap::default();
    let mut module = Module::default();
    let mut registry = TypeRegistry::new(&mut module, &mut types);
    registry.register_type::<MyStruct>();
    let ty = module.types.insert(
        Type {
            inner: TypeInner::Scalar {
                kind: naga::ScalarKind::Sint,
                width: 4,
            },
            name: None,
        },
        SPAN,
    );
    let mut function = Function::default();
    function.arguments.push(FunctionArgument {
        name: Some("oh_neat".to_string()),
        ty,
        binding: Some(naga::Binding::Location {
            location: 0,
            interpolation: None,
            sampling: None,
        }),
    });
    let arg_expr = function
        .expressions
        .append(Expression::FunctionArgument(0), SPAN);
    // let arg_load_expr = function
    //     .expressions
    //     .append(Expression::Load { pointer: arg_expr }, SPAN);
    let named = function.expressions.append(
        naga::Expression::Unary {
            op: naga::UnaryOperator::Negate,
            expr: arg_expr,
        },
        SPAN,
    );
    function.named_expressions.insert(named, "val".to_string());
    function
        .body
        .push(Statement::Emit(function.expressions.range_from(0)), SPAN);
    let entry_point = EntryPoint {
        name: "main".to_string(),
        stage: naga::ShaderStage::Compute,
        early_depth_test: None,
        workgroup_size: [256, 1, 1],
        function,
    };
    module.entry_points.push(entry_point);
    return module;
}
