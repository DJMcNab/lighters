use std::mem::size_of;

use bytemuck::cast_slice;
use wgpu::{
    util::{BufferInitDescriptor, DeviceExt},
    BindGroupDescriptor, BufferDescriptor, BufferUsages, CommandEncoderDescriptor,
    ComputePipelineDescriptor, Device, DeviceDescriptor, Limits, PowerPreference, Queue,
    RequestAdapterOptions, ShaderModule,
};

pub struct GpuResources {
    pub device: Device,
    pub queue: Queue,
}

impl GpuResources {
    pub async fn setup() -> Self {
        let instance = wgpu::Instance::default();
        let adapter = instance
            .request_adapter(&RequestAdapterOptions {
                power_preference: PowerPreference::HighPerformance,
                force_fallback_adapter: false,
                compatible_surface: None,
            })
            .await
            .unwrap();
        let (device, queue) = adapter
            .request_device(
                &DeviceDescriptor {
                    label: Some("Compute Device"),
                    features: wgpu::Features::default(),
                    limits: Limits::default(),
                },
                None,
            )
            .await
            .unwrap();
        GpuResources { device, queue }
    }

    pub async fn run_collatz(&self, module: &ShaderModule) -> Vec<u32> {
        // Heavily adapted from https://github.com/gfx-rs/wgpu/blob/trunk/wgpu/examples/hello-compute/main.rs
        // Used under MIT license
        // see examples/wgsl/src/collatz.wgsl for license text
        let input_data: Vec<u32> = (1..=256 * 1024).collect(); // 1MiB
        let size = (input_data.len() * size_of::<u32>()) as u64;
        let main_buffer = self.device.create_buffer_init(&BufferInitDescriptor {
            label: Some("collatz_buffer"),
            contents: cast_slice(&input_data),
            usage: BufferUsages::STORAGE | BufferUsages::COPY_SRC, // To read the contents
        });

        // This buffer gets the data after the computation has been run
        let staging_buffer = self.device.create_buffer(&BufferDescriptor {
            label: None,
            size,
            usage: BufferUsages::MAP_READ | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let pipeline = self
            .device
            .create_compute_pipeline(&ComputePipelineDescriptor {
                label: None,
                layout: None,
                module,
                entry_point: "main",
            });
        let bind_group_layout = pipeline.get_bind_group_layout(0);
        let bind_group = self.device.create_bind_group(&BindGroupDescriptor {
            label: None,
            layout: &bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: main_buffer.as_entire_binding(),
            }],
        });
        let mut encoder = self
            .device
            .create_command_encoder(&CommandEncoderDescriptor { label: None });
        {
            let mut pass = encoder.begin_compute_pass(&Default::default());
            pass.set_pipeline(&pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.dispatch_workgroups((input_data.len() as u32) / 64, 1, 1);
        }
        encoder.copy_buffer_to_buffer(&main_buffer, 0, &staging_buffer, 0, size);
        self.queue.submit(Some(encoder.finish()));
        let slice = staging_buffer.slice(..);
        let (sender, reciever) = futures_intrusive::channel::shared::oneshot_channel();
        slice.map_async(wgpu::MapMode::Read, move |v| sender.send(v).unwrap());
        self.device.poll(wgpu::Maintain::Wait);

        if let Some(Ok(())) = reciever.receive().await {
            let data = slice.get_mapped_range();
            let result: Vec<u32> = cast_slice(&data).to_vec();
            drop(data);
            staging_buffer.unmap();
            result
        } else {
            panic!("Failed to get collatz data from module");
        }
    }
}
