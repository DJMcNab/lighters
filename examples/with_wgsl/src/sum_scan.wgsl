// Adapted from https://github.com/linebender/vello/blob/main/shader/pathtag_scan.wgsl
// used under the MIT license, which is provided at the bottom of this file.

@group(0)
@binding(0)
var<storage, read_write> in_data: array<u32>; // this is used as both input and output for convenience

@group(0)
@binding(1)
var<storage, read> reduced: array<u32>; // this is used as both input and output for convenience

var<workgroup> scratch: array<u32, 256>;

@compute
@workgroup_size(256)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>, @builtin(local_invocation_id) local_id: vec3<u32>, @builtin(workgroup_id) wg_id: vec3<u32>) {
    let ix = global_id.x;
    var agg = in_data[ix];
    if local_id.x == 255u && wg_id.x < 255u {
        agg = agg + reduced[wg_id.x + 1u];
    }
    scratch[local_id.x] = agg;
    for (var i = 0u; i < 8u; i += 1u) {
        workgroupBarrier();
        if local_id.x + (1u << i) < 256u {
            let other = scratch[local_id.x + (1u << i)];
            agg = agg + other;
        }
        workgroupBarrier();
        scratch[local_id.x] = agg;
    }
    in_data[global_id.x] = agg;
}

// Copyright (c) 2020 Raph Levien
// 
// Permission is hereby granted, free of charge, to any
// person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the
// Software without restriction, including without
// limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following
// conditions:
// 
// The above copyright notice and this permission notice
// shall be included in all copies or substantial portions
// of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
