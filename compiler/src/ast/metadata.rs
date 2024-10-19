use std::{
    any::{type_name, Any, TypeId},
    collections::HashMap,
    fmt::Debug,
    sync::Arc,
};

use serde::Serialize;

pub(crate) trait AnyDebug: Any + Debug {
    fn as_any(&self) -> &dyn Any;
}

impl<T> AnyDebug for T
where
    T: Any + Debug,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Default, Clone, Debug)]
pub(crate) struct Metadata {
    map: HashMap<TypeId, (Arc<dyn AnyDebug>, &'static str)>,
}

impl Serialize for Metadata {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_map(
            self.map
                .values()
                .map(|(value, type_name)| (type_name, format!("{value:?}"))),
        )
    }
}

impl Metadata {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set<T: 'static + Debug>(&mut self, value: T) {
        let arced: Arc<dyn AnyDebug> = Arc::new(value);
        let type_id = TypeId::of::<T>();

        self.map.insert(type_id, (arced, type_name::<T>()));
    }

    pub fn get<T: 'static + Debug>(&self) -> Option<&T> {
        let type_id = TypeId::of::<T>();
        self.map
            .get(&type_id)
            .and_then(|(value, _)| (**value).as_any().downcast_ref())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn store_and_retrieve() {
        #[derive(Debug, PartialEq)]
        struct SomeData(i32);

        let mut meta = Metadata::new();

        meta.set(SomeData(33));

        assert_eq!(meta.get::<SomeData>(), Some(&SomeData(33)));
    }
}
