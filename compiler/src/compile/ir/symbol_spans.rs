use std::collections::HashMap;

use crate::ast::SymbolId;
use crate::tokens::Span;

/// Tracks the source location where each symbol's value was computed.
///
/// This enables precise diagnostics that point to the problematic subexpression
/// (e.g., `y - y` in a division-by-zero) rather than the operation itself.
#[derive(Debug, Clone, Default)]
pub struct SymbolSpans {
    spans: HashMap<SymbolId, Span>,
}

impl SymbolSpans {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record the definition span for a symbol.
    pub fn define(&mut self, symbol: SymbolId, span: Span) {
        self.spans.insert(symbol, span);
    }

    /// Get the definition span for a symbol, if known.
    pub fn get(&self, symbol: SymbolId) -> Option<Span> {
        self.spans.get(&symbol).copied()
    }

    /// Create an alias: new_symbol gets the same span as existing_symbol.
    ///
    /// Used during SSA conversion when symbols are renamed.
    pub fn alias(&mut self, new_symbol: SymbolId, existing_symbol: SymbolId) {
        if let Some(span) = self.spans.get(&existing_symbol).copied() {
            self.spans.insert(new_symbol, span);
        }
    }

    /// Merge spans from another SymbolSpans into this one.
    pub fn extend(&mut self, other: &SymbolSpans) {
        self.spans.extend(other.spans.iter().map(|(k, v)| (*k, *v)));
    }
}

#[cfg(test)]
mod test {
    use crate::tokens::FileId;

    use super::*;

    fn make_span(start: usize, end: usize) -> Span {
        Span::new(FileId::new(0), start, end)
    }

    #[test]
    fn test_define_and_get() {
        let mut spans = SymbolSpans::new();
        let symbol = SymbolId(42);
        let span = make_span(10, 20);

        spans.define(symbol, span);
        assert_eq!(spans.get(symbol), Some(span));
    }

    #[test]
    fn test_get_unknown_symbol() {
        let spans = SymbolSpans::new();
        assert_eq!(spans.get(SymbolId(999)), None);
    }

    #[test]
    fn test_alias() {
        let mut spans = SymbolSpans::new();
        let original = SymbolId(1);
        let renamed = SymbolId(2);
        let span = make_span(5, 15);

        spans.define(original, span);
        spans.alias(renamed, original);

        assert_eq!(spans.get(renamed), Some(span));
    }

    #[test]
    fn test_alias_unknown_symbol() {
        let mut spans = SymbolSpans::new();
        let original = SymbolId(1);
        let renamed = SymbolId(2);

        // Aliasing an unknown symbol should not create an entry
        spans.alias(renamed, original);
        assert_eq!(spans.get(renamed), None);
    }

    #[test]
    fn test_extend() {
        let mut spans1 = SymbolSpans::new();
        let mut spans2 = SymbolSpans::new();

        spans1.define(SymbolId(1), make_span(0, 5));
        spans2.define(SymbolId(2), make_span(10, 15));

        spans1.extend(&spans2);

        assert_eq!(spans1.get(SymbolId(1)), Some(make_span(0, 5)));
        assert_eq!(spans1.get(SymbolId(2)), Some(make_span(10, 15)));
    }
}
