pub mod array_try_map;

use hecs::{Bundle, CommandBuffer, Query, World};

pub fn insert_default_bundles<B: Bundle + Default, Q: Query>(world: &mut World) {
    let mut cmdbuf = CommandBuffer::new();

    for (entity, ()) in world.query_mut::<()>().with::<Q>() {
        cmdbuf.insert(entity, B::default());
    }

    cmdbuf.run_on(world);
}
