use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ExpressionKind, SymbolId},
    CompileSettings,
};

use super::Constant;

#[derive(Debug, Default)]
pub(crate) struct ConstantPropagationMap {
    map: HashMap<SymbolId, Constant>,
    poisoned: HashSet<SymbolId>,
}

impl ConstantPropagationMap {
    pub fn apply_poisons(&mut self, other: &ConstantPropagationMap) {
        self.poisoned.extend(&other.poisoned);
        self.map
            .retain(|symbol_id, _| !other.poisoned.contains(symbol_id));
    }

    pub fn set(&mut self, symbol_id: SymbolId, value: &ExpressionKind) {
        self.poisoned.insert(symbol_id);

        if let Ok(value) = value.try_into() {
            self.map.insert(symbol_id, value);
        } else {
            self.map.remove(&symbol_id);
        }
    }

    pub fn get(&self, symbol_id: SymbolId) -> Option<Constant> {
        self.map.get(&symbol_id).copied()
    }

    pub fn poison_properties(&mut self, compile_settings: &CompileSettings) {
        self.map.retain(|&symbol_id, _| {
            if compile_settings.is_property(symbol_id) {
                self.poisoned.insert(symbol_id);
                false
            } else {
                true
            }
        });
    }

    pub fn snapshot(&self) -> Self {
        Self {
            map: self.map.clone(),
            poisoned: HashSet::new(),
        }
    }
}
