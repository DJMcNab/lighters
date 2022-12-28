use naga::{
    back::wgsl::WriterFlags,
    valid::{Capabilities, ValidationFlags},
};

fn main() {
    let module = lighters::module();

    let writer_flags = WriterFlags::all();
    let validation_flags = ValidationFlags::all();
    let capabilities =
        // Taken from naga CLI for wgsl input
        Capabilities::all() & !(Capabilities::CLIP_DISTANCE | Capabilities::CULL_DISTANCE);
    let mut validator = naga::valid::Validator::new(validation_flags, capabilities);
    let module_info = validator.validate(&module).unwrap();
    let result = naga::back::wgsl::write_string(&module, &module_info, writer_flags).unwrap();
    std::fs::write("output.wgsl", result).unwrap();
}
