use std::borrow::Cow;

use lighters::{
    naga::{
        back::wgsl::{self, WriterFlags},
        valid::{Capabilities, ValidationFlags, Validator},
        Module,
    },
    types::ReadWrite,
    BlockContext, ModuleContext, Returned, Value,
};

/// Run the examples with WGSL shaders, to test compilation time and
/// Binary size when using Naga's translation

fn write_module(module: &Module) {
    let writer_flags = WriterFlags::all();
    let validation_flags = ValidationFlags::all();
    let capabilities =
        // Taken from naga CLI for wgsl input
        Capabilities::all() & !(Capabilities::CLIP_DISTANCE | Capabilities::CULL_DISTANCE);
    let mut validator = Validator::new(validation_flags, capabilities);
    let module_info = validator.validate(&module).unwrap();
    let result = wgsl::write_string(&module, &module_info, writer_flags).unwrap();
    std::fs::write(concat!(env!("CARGO_MANIFEST_DIR"), "/output.wgsl"), result).unwrap();
}

fn main() {
    pollster::block_on(run());
}

async fn run() {
    let setup = runtime::GpuResources::setup().await;
    let module = collatz_module();
    write_module(&module);
    let module = setup
        .device
        .create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Naga(Cow::Owned(module)),
        });
    let result = setup.run_collatz(&module).await;
    println!("{:?}", &result[..32]);
}

fn collatz_module() -> Module {
    let mut module_cx = ModuleContext::default();
    module_cx.entry_point("main", [64, 1, 1], |epcx| {
        let id = epcx.global_invocation_id();
        let in_out = epcx.storage_variable::<Box<[u32]>, ReadWrite>(
            "results_in_out",
            lighters::naga::ResourceBinding {
                group: 0,
                binding: 0,
            },
        );
        epcx.body(|cx| {
            let idx = id.inner().get_component(0);
            let val_ptr = in_out.index(idx);
            let collatz_result = cx.call_function(collatz_iterations, &(val_ptr.load(),));
            cx.store(&val_ptr, &collatz_result);
        });
    });
    module_cx.module()
}

fn collatz_iterations(cx: &mut BlockContext, n_base: Value<u32>) -> Returned<u32> {
    let n = cx.local_variable("n");
    let i = cx.local_variable_init("i", 0);
    cx.store(&n, &n_base);
    cx.loop_(|cx| {
        cx.if_(&n.load().le(&cx.const_(1)), |cx| cx.break_(), |_| {});
        cx.if_(
            &(n.load() % 2).eq(&cx.const_(0)),
            |cx| cx.store(&n, &(n.load() / 2)),
            |cx| {
                n.load().saturating_mul(
                    &cx.const_(3),
                    cx,
                    &n,
                    Some(|cx: &mut BlockContext| cx.return_(&n.load()).into()),
                );
                cx.store(&n, &(n.load() + 1));
            },
        );
        cx.store(&i, &(i.load() + 1));
    });

    cx.return_(&i.load())
}
