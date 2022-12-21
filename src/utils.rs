pub mod array_try_map;

use hecs::{DynamicBundle, Query, World};

pub fn insert_default_bundles<B: DynamicBundle + Default, Q: Query>(world: &mut World) {
    let entities = world
        .query_mut::<()>()
        .with::<Q>()
        .into_iter()
        .map(|(entity, ())| entity)
        .collect::<Vec<_>>();

    for entity in entities {
        world.insert(entity, B::default()).unwrap();
    }
}
