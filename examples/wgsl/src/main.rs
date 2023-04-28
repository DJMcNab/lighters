/// Run the examples with WGSL shaders, to test compilation time and
/// Binary size when using Naga's translation

fn main() {
    pollster::block_on(run());
}

async fn run() {
    let setup = runtime::GpuResources::setup().await;
    let module = setup
        .device
        .create_shader_module(wgpu::include_wgsl!("./collatz.wgsl"));
    let result = setup.run_collatz(&module).await;
    println!("{:?}", &result[..32]);
}
