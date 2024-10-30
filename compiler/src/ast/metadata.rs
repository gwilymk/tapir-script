use std::{
    any::{type_name, Any, TypeId},
    collections::HashMap,
    fmt::Debug,
};

use serde::Serialize;

pub(crate) trait AnyDebug: Any + Debug {
    fn as_any(&self) -> &dyn Any;
    fn clone_to_any(&self) -> Box<dyn AnyDebug>;
}

impl<T> AnyDebug for T
where
    T: Any + Debug + Clone,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_to_any(&self) -> Box<dyn AnyDebug> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn AnyDebug> {
    fn clone(&self) -> Self {
        self.clone_to_any()
    }
}

#[derive(Default, Clone)]
pub(crate) struct Metadata {
    map: HashMap<TypeId, (Box<dyn AnyDebug>, &'static str)>,
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

impl Debug for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut entries: Vec<_> = self
            .map
            .values()
            .map(|(kind, _)| format!("{kind:?}"))
            .collect();
        entries.sort();

        f.debug_set().entries(entries).finish()
    }
}

impl Metadata {
    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn set<T: 'static + Debug + Clone>(&mut self, value: T) {
        let type_id = TypeId::of::<T>();

        self.map
            .insert(type_id, (Box::new(value), type_name::<T>()));
    }

    pub fn get<T: 'static + Debug>(&self) -> Option<&T> {
        let type_id = TypeId::of::<T>();
        self.map
            .get(&type_id)
            .and_then(|(value, _)| (**value).as_any().downcast_ref())
    }

    pub fn has<T: 'static + Debug>(&self) -> bool {
        let type_id = TypeId::of::<T>();
        self.map
            .get(&type_id)
            .map_or(false, |(value, _)| (**value).as_any().is::<T>())
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn store_and_retrieve() {
        #[derive(Debug, PartialEq, Clone, Copy)]
        struct SomeData(i32);

        let mut meta = Metadata::new();

        meta.set(SomeData(33));

        assert_eq!(meta.get::<SomeData>(), Some(&SomeData(33)));
    }
}
