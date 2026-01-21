use compiler::SourceRange;
use lsp_types::{Position, Range};

pub fn position_to_offset(text: &str, position: Position) -> Option<usize> {
    let mut line = 0;
    let mut col = 0;

    for (i, ch) in text.char_indices() {
        if line == position.line && col == position.character {
            return Some(i);
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    // Handle position at end of file
    if line == position.line && col == position.character {
        return Some(text.len());
    }

    None
}

pub fn offset_to_position(text: &str, offset: usize) -> Option<Position> {
    if offset > text.len() {
        return None;
    }

    let mut line = 0;
    let mut col = 0;

    for (i, ch) in text.char_indices() {
        if i == offset {
            return Some(Position::new(line, col));
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    // Handle offset at end of file
    if offset == text.len() {
        return Some(Position::new(line, col));
    }

    None
}

pub fn source_range_to_lsp_range(range: SourceRange) -> Range {
    Range {
        start: Position::new(range.start.line as u32, range.start.column as u32),
        end: Position::new(range.end.line as u32, range.end.column as u32),
    }
}
