use std::{
    any::{type_name, Any, TypeId},
    collections::HashMap,
    sync::Arc,
};

use serde::Serialize;

#[derive(Default, Clone, Debug)]
pub(crate) struct Metadata {
    map: HashMap<TypeId, (Arc<dyn Any>, &'static str)>,
}

impl Serialize for Metadata {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut type_names: Vec<_> = self.map.values().map(|(_, name)| name).collect();
        type_names.sort_unstable();
        type_names.serialize(serializer)
    }
}

impl Metadata {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set<T: 'static>(&mut self, value: T) {
        let arced: Arc<dyn Any> = Arc::new(value);
        let type_id = arced.type_id();

        let type_name = type_name::<T>();

        self.map.insert(type_id, (arced, type_name));
    }

    pub fn get<T: 'static>(&self) -> Option<&T> {
        let type_id = TypeId::of::<T>();
        self.map
            .get(&type_id)
            .and_then(|(value, _)| value.downcast_ref())
    }
}
