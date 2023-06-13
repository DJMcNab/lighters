use std::borrow::Cow;

use lighters::{
    algorithms::Sum,
    glam::{Vec2, Vec4},
    naga::Module,
    types::{ReadOnly, ReadWrite},
    BlockContext, ModuleContext, Returned, ToType, Value,
};
use naga::{StructMember, TypeInner};

/// Run the examples with WGSL shaders, to test compilation time and
/// Binary size when using Naga's translation

fn write_module(module: &Module) {
    use lighters::naga::valid::{Capabilities, ValidationFlags, Validator};
    let writer_flags = lighters::naga::back::wgsl::WriterFlags::all();
    let validation_flags = ValidationFlags::empty();
    let capabilities =
        // Taken from naga CLI for wgsl input
        Capabilities::all() & !(Capabilities::CLIP_DISTANCE | Capabilities::CULL_DISTANCE);
    let mut validator = Validator::new(validation_flags, capabilities);
    let module_info = validator.validate(module).unwrap();
    let result =
        lighters::naga::back::wgsl::write_string(module, &module_info, writer_flags).unwrap();
    std::fs::write(concat!(env!("CARGO_MANIFEST_DIR"), "/output.wgsl"), result).unwrap();
}

fn main() {
    pollster::block_on(run());
}

async fn run() {
    let setup = runtime::GpuResources::setup().await;
    let mut args = std::env::args();
    args.next();
    match &args.next().as_deref() {
        Some("collatz") => {
            let start = std::time::Instant::now();
            let module = collatz_module();
            // write_module(&module);
            let module = setup
                .device
                .create_shader_module(wgpu::ShaderModuleDescriptor {
                    label: None,
                    source: wgpu::ShaderSource::Naga(Cow::Owned(module)),
                });
            eprintln!("Shader setup took {:.3?}", start.elapsed());
            let result = setup.run_collatz(&module).await;
            println!("{:?}", &result[..32]);
        }
        Some("prefix_sum") => {
            let start = std::time::Instant::now();
            let reduce_module = reduce_module();
            let reduce = setup
                .device
                .create_shader_module(wgpu::ShaderModuleDescriptor {
                    label: None,
                    source: wgpu::ShaderSource::Naga(Cow::Owned(reduce_module)),
                });
            let reduce2_module = reduce2_module();
            let reduce2 = setup
                .device
                .create_shader_module(wgpu::ShaderModuleDescriptor {
                    label: None,
                    source: wgpu::ShaderSource::Naga(Cow::Owned(reduce2_module)),
                });
            let scan_module = scan_module();
            // write_module(&scan_module);
            let scan = setup
                .device
                .create_shader_module(wgpu::ShaderModuleDescriptor {
                    label: None,
                    source: wgpu::ShaderSource::Naga(Cow::Owned(scan_module)),
                });
            eprintln!("Shader setup took {:.3?}", start.elapsed());
            let result = setup.run_prefix_sum(&reduce, &reduce2, &scan).await;
            println!("{:?}", &result[..32]);
        }
        _ => {
            panic!("Must select either `collatz` or `prefix_sum` as argument")
        }
    }
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
            let idx = id.inner().x();
            let val_ptr = in_out.index(idx);
            let collatz_result = cx.call_function(collatz_iterations, &(val_ptr.load(),));
            cx.store(val_ptr, collatz_result);
        });
    });
    module_cx.module()
}

fn collatz_iterations(cx: &mut BlockContext, n_base: Value<u32>) -> Returned<u32> {
    let n = cx.local_variable("n");
    let i = cx.local_variable_init("i", 0);
    cx.store(n, n_base);
    cx.loop_(|cx| {
        cx.if_(n.load().le(1), |cx| cx.break_(), |_| {});
        cx.if_(
            (n.load() % 2).eq(0),
            |cx| cx.store(n, n.load() / 2),
            |cx| {
                n.load().saturating_mul(
                    3,
                    cx,
                    n,
                    Some(|cx: &mut BlockContext| cx.return_(n.load()).into()),
                );
                cx.store(n, n.load() + 1);
            },
        );
        cx.store(i, i.load() + 1);
    });

    i.load().as_return()
}

fn reduce_module() -> Module {
    let mut module_cx = ModuleContext::default();
    module_cx.entry_point("main", [256, 1, 1], |epcx| {
        let input = epcx.storage_variable::<Box<[u32]>, ReadOnly>(
            "input",
            lighters::naga::ResourceBinding {
                group: 0,
                binding: 0,
            },
        );
        let reduced = epcx.storage_variable::<Box<[u32]>, ReadWrite>(
            "reduced_out",
            lighters::naga::ResourceBinding {
                group: 0,
                binding: 1,
            },
        );
        let local_id = epcx.local_invocation_id();
        let scratch = epcx.workgroup_variable("scratch");
        let global_id = epcx.global_invocation_id();
        let wg_id = epcx.work_group_id();
        epcx.body(|cx| {
            let initial = input.index(global_id.inner().x()).load();
            let this_res = cx.call_function(
                lighters::algorithms::reverse_scan::<u32, Sum, 256>,
                &(local_id.clone(), scratch, initial),
            );
            cx.if_(
                local_id.inner().x().eq(0),
                |cx| {
                    cx.store(reduced.index(wg_id.inner().x()), this_res);
                },
                |_| {},
            );
        });
    });
    module_cx.module()
}

fn reduce2_module() -> Module {
    let mut module_cx = ModuleContext::default();
    module_cx.entry_point("main", [256, 1, 1], |epcx| {
        let input = epcx.storage_variable::<Box<[u32]>, ReadWrite>(
            "input",
            lighters::naga::ResourceBinding {
                group: 0,
                binding: 0,
            },
        );
        let local_id = epcx.local_invocation_id();
        let scratch = epcx.workgroup_variable("scratch");
        epcx.body(|cx| {
            let initial = input.index(local_id.inner().x()).load();
            let this_res = cx.call_function(
                lighters::algorithms::reverse_scan::<u32, Sum, 256>,
                &(local_id.clone(), scratch, initial),
            );
            cx.store(input.index(local_id.inner().x()), this_res);
        });
    });
    module_cx.module()
}

fn scan_module() -> Module {
    let mut module_cx = ModuleContext::default();
    module_cx.entry_point("main", [256, 1, 1], |epcx| {
        let in_out_results = epcx.storage_variable::<Box<[u32]>, ReadWrite>(
            "in_out_results",
            lighters::naga::ResourceBinding {
                group: 0,
                binding: 0,
            },
        );
        let reduced = epcx.storage_variable::<Box<[u32]>, ReadOnly>(
            "reduced_in",
            lighters::naga::ResourceBinding {
                group: 0,
                binding: 1,
            },
        );
        let local_id = epcx.local_invocation_id();
        let scratch = epcx.workgroup_variable("scratch");
        let global_id = epcx.global_invocation_id();
        let wg_id = epcx.work_group_id();
        epcx.body(|cx| {
            let initial = cx.local_variable("initial");
            cx.store(initial, in_out_results.index(global_id.inner().x()).load());
            cx.if_(
                local_id.inner().x().eq(255),
                |cx| {
                    cx.if_(
                        wg_id.inner().x().lt(255),
                        |cx| {
                            cx.store(
                                initial,
                                initial.load() + reduced.index(wg_id.inner().x() + 1).load(),
                            );
                        },
                        |_| {},
                    );
                },
                |_| {},
            );
            let this_res = cx.call_function(
                lighters::algorithms::reverse_scan::<u32, Sum, 256>,
                &(local_id.clone(), scratch, initial.load()),
            );
            cx.store(in_out_results.index(global_id.inner().x()), this_res);
        });
    });
    module_cx.module()
}

struct LineSegment {
    start: Vec2,
    end: Vec2,
    color: Vec4,
}

impl ToType for LineSegment {
    fn naga_ty_inner(registry: &mut lighters::TypeRegistry) -> naga::TypeInner {
        TypeInner::Struct {
            members: vec![
                StructMember {
                    name: None,
                    ty: registry.register_type::<Vec2>(),
                    binding: None,
                    offset: 0,
                },
                StructMember {
                    name: None,
                    ty: registry.register_type::<Vec2>(),
                    binding: None,
                    offset: 8,
                },
                StructMember {
                    name: None,
                    ty: registry.register_type::<Vec4>(),
                    binding: None,
                    offset: 16,
                },
            ],
            span: 32,
        }
    }
}
