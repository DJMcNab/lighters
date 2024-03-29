// Taken from https://github.com/gfx-rs/wgpu/blob/trunk/wgpu/examples/hello-compute/shader.wgsl
// used under the MIT license, which is provided at the bottom of this file.


// The Collatz Conjecture states that for any integer n:
// If n is even, n = n/2
// If n is odd, n = 3n+1
// And repeat this process for each new n, you will always eventually reach 1.
// Though the conjecture has not been proven, no counterexample has ever been found.
// This function returns how many times this recurrence needs to be applied to reach 1.
fn collatz_iterations(n_base: u32) -> u32 {
    var n: u32 = n_base;
    var i: u32 = 0u;
    loop {
        if n <= 1u {
            break;
        }
        if n % 2u == 0u {
            n = n / 2u;
        } else {
            // Overflow? (i.e. 3*n + 1 > 0xffffffffu?)
            if n >= 1431655765u {   // 0x55555555u
                return 4294967295u;   // 0xffffffffu
            }

            n = 3u * n + 1u;
        }
        i = i + 1u;
    }
    return i;
}

@group(0)
@binding(0)
// this is used as both input and output for convenience
var<storage, read_write> input_results: array<u32>; 

@compute
@workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let res = collatz_iterations(input_results[global_id.x]);
    input_results[global_id.x] = res;
}


// Copyright (c) [yyyy] [name of copyright owner]
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
