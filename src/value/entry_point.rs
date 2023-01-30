use glam::UVec3;

use crate::wrapper;

wrapper!(struct GlobalInvocationId: UVec3);
wrapper!(struct LocalInvocationId: UVec3);

wrapper!(struct LocalInvocationIndex: u32);
wrapper!(struct WorkGroupId: UVec3);
wrapper!(struct NumWorkGroups: UVec3);

wrapper!(struct WorkgroupSize: UVec3);
