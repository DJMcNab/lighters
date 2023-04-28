use std::borrow::Cow;

use lighters::{naga::Module, BlockContext, ModuleContext, Returned, Value};

/// Run the examples with WGSL shaders, to test compilation time and
/// Binary size when using Naga's translation

fn main() {
    pollster::block_on(run());
}

async fn run() {
    let setup = runtime::GpuResources::setup().await;
    let module = setup
        .device
        .create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Naga(Cow::Owned(collatz_module())),
        });
    let result = setup.run_collatz(&module).await;
    println!("{:?}", &result[..32]);
}

fn collatz_module() -> Module {
    let mut module_cx = ModuleContext::default();
    module_cx.entry_point("main", [64, 1, 1], |epcx| {
        let id = epcx.global_invocation_id();
        epcx.body(|cx| {
            let collatz_result =
                cx.call_function(collatz_iterations, &(id.inner().get_component(0),));
        });
    });
    module_cx.module()
}

fn collatz_iterations(cx: &mut BlockContext, n_base: Value<u32>) -> Returned<u32> {
    let n = cx.local_variable("n");
    let i = cx.local_variable_init("i", 0);
    cx.store(&n, &n_base);
    cx.loop_(|cx| {
        cx.if_(&n.load().le(cx.const_(1)), |cx| cx.break_(), |_| {});
        cx.if_(
            &(n.load() % 2).eq(cx.const_(0)),
            |cx| cx.store(&n, &(n.load() / 2)),
            |cx| {
                cx.if_(
                    &n.load().ge(cx.const_((u32::MAX - 1) / 3)),
                    |cx| cx.return_(&cx.const_(u32::MAX)).into(),
                    |_| {},
                );
            },
        )
    });

    cx.return_(&i.load())
}
