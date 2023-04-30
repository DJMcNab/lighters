/// Run the examples with WGSL shaders, to test compilation time and
/// Binary size when using Naga's translation

fn main() {
    pollster::block_on(run());
}

async fn run() {
    let setup = runtime::GpuResources::setup().await;
    let mut args = std::env::args();
    args.next();
    match &args.next().as_deref() {
        Some("collatz") => {
            let module = setup
                .device
                .create_shader_module(wgpu::include_wgsl!("./collatz.wgsl"));
            let result = setup.run_collatz(&module).await;
            println!("{:?}", &result[..32]);
        }
        Some("prefix_sum") => {
            let start = std::time::Instant::now();

            let reduce = setup
                .device
                .create_shader_module(wgpu::include_wgsl!("./sum_reduce.wgsl"));
            let reduce2 = setup
                .device
                .create_shader_module(wgpu::include_wgsl!("./sum_reduce2.wgsl"));
            let scan = setup
                .device
                .create_shader_module(wgpu::include_wgsl!("./sum_scan.wgsl"));
            eprintln!("Shader setup took {:.3?}", start.elapsed());

            let result = setup.run_prefix_sum(&reduce, &reduce2, &scan).await;
            println!("{:?}", &result[..32]);
        }
        _ => {
            panic!("Must select either `collatz` or `prefix_sum` as argument")
        }
    }
}
