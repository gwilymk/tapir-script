use std::collections::HashMap;

use lsp_types::{
    GotoDefinitionResponse, Hover, HoverContents, InlayHint, InlayHintLabel, Location,
    MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, Position,
    PrepareRenameResponse, SignatureHelp, SignatureInformation, TextEdit, Url, WorkspaceEdit,
};

use crate::state::FileState;
use crate::util::{offset_to_position, position_to_offset, source_range_to_lsp_range};

pub fn find_definition(
    file_state: &mut FileState,
    uri: &Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    // Convert position to byte offset
    let offset = position_to_offset(&file_state.text, position)?;

    // Find which reference span contains this offset
    for (usage_span, def_span) in &file_state.analysis.references {
        if usage_span.contains_offset(offset) {
            // Found the reference, convert definition span to location
            let range = file_state
                .analysis
                .diagnostics
                .span_to_range(*def_span)
                .map(source_range_to_lsp_range)?;

            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            }));
        }
    }

    None
}

pub fn find_hover(file_state: &mut FileState, position: Position) -> Option<Hover> {
    // Convert position to byte offset
    let offset = position_to_offset(&file_state.text, position)?;

    // Find which span contains this offset
    for (span, info) in &file_state.analysis.hover_info {
        if span.contains_offset(offset) {
            let range = file_state
                .analysis
                .diagnostics
                .span_to_range(*span)
                .map(source_range_to_lsp_range);

            let value = if let Some(doc) = &info.doc_comment {
                format!("```tapir\n{}\n```\n\n{}", info.signature, doc)
            } else {
                format!("```tapir\n{}\n```", info.signature)
            };

            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range,
            });
        }
    }

    None
}

pub fn get_inlay_hints(file_state: &FileState) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    // Type hints for variable declarations
    for hint in &file_state.analysis.inlay_hints {
        if let Some(position) = offset_to_position(&file_state.text, hint.position) {
            hints.push(InlayHint {
                position,
                label: InlayHintLabel::String(hint.label.clone()),
                kind: Some(lsp_types::InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            });
        }
    }

    // Parameter name hints for function calls
    for call_site in &file_state.analysis.call_sites {
        if let Some(sig) = file_state.analysis.signatures.get(&call_site.function_name) {
            for (i, &start_offset) in call_site.argument_start_offsets.iter().enumerate() {
                // Get the parameter name (just the name, not the type)
                if let Some(param) = sig.parameters.get(i) {
                    let param_name = param.label.split(':').next().unwrap_or(&param.label).trim();
                    if let Some(position) = offset_to_position(&file_state.text, start_offset) {
                        hints.push(InlayHint {
                            position,
                            label: InlayHintLabel::String(format!("{}:", param_name)),
                            kind: Some(lsp_types::InlayHintKind::PARAMETER),
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            padding_right: Some(true),
                            data: None,
                        });
                    }
                }
            }
        }
    }

    hints
}

pub fn find_signature_help(file_state: &FileState, position: Position) -> Option<SignatureHelp> {
    let offset = position_to_offset(&file_state.text, position)?;

    // Use text-based scanning to find function call context
    // This works even when the code doesn't parse yet
    let (function_name, active_parameter) = find_call_context(&file_state.text, offset)?;

    // Look up the signature
    let sig_info = file_state.analysis.signatures.get(&function_name)?;

    let parameters: Vec<ParameterInformation> = sig_info
        .parameters
        .iter()
        .map(|p| ParameterInformation {
            label: ParameterLabel::Simple(p.label.clone()),
            documentation: None,
        })
        .collect();

    let active_param = active_parameter.min(sig_info.parameters.len().saturating_sub(1)) as u32;

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label: sig_info.label.clone(),
            documentation: None,
            parameters: Some(parameters),
            active_parameter: Some(active_param),
        }],
        active_signature: Some(0),
        active_parameter: Some(active_param),
    })
}

/// Scan backwards from cursor to find function call context.
/// Returns (function_name, parameter_index) if inside a function call.
fn find_call_context(text: &str, offset: usize) -> Option<(String, usize)> {
    let bytes = text.as_bytes();
    if offset > bytes.len() {
        return None;
    }

    let mut paren_depth = 0;
    let mut comma_count = 0;
    let mut pos = offset;

    // Scan backwards to find the opening parenthesis
    while pos > 0 {
        pos -= 1;
        let ch = bytes[pos] as char;

        match ch {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth == 0 {
                    // Found the opening paren, now find the function name
                    let name = extract_identifier_before(text, pos)?;
                    return Some((name, comma_count));
                }
                paren_depth -= 1;
            }
            ',' if paren_depth == 0 => comma_count += 1,
            // Stop at statement boundaries
            ';' | '{' | '}' => return None,
            _ => {}
        }
    }

    None
}

/// Extract an identifier immediately before the given position (skipping whitespace).
fn extract_identifier_before(text: &str, pos: usize) -> Option<String> {
    let bytes = text.as_bytes();
    let mut end = pos;

    // Skip whitespace backwards
    while end > 0 && (bytes[end - 1] as char).is_whitespace() {
        end -= 1;
    }

    if end == 0 {
        return None;
    }

    // Find the start of the identifier
    let mut start = end;
    while start > 0 {
        let ch = bytes[start - 1] as char;
        if ch.is_alphanumeric() || ch == '_' {
            start -= 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    // Check that the first character is valid for an identifier (not a digit)
    let first_char = bytes[start] as char;
    if first_char.is_ascii_digit() {
        return None;
    }

    Some(text[start..end].to_string())
}

pub fn find_references(
    file_state: &mut FileState,
    uri: &Url,
    position: Position,
    include_declaration: bool,
) -> Option<Vec<Location>> {
    let offset = position_to_offset(&file_state.text, position)?;

    // First, find the definition span for the symbol at cursor
    // The cursor could be on a usage (look up in references) or on a definition itself
    let def_span = {
        // Check if cursor is on a usage span
        let mut found_def = None;
        for (usage_span, def_span) in &file_state.analysis.references {
            if usage_span.contains_offset(offset) {
                found_def = Some(*def_span);
                break;
            }
        }

        // If not found as usage, check if cursor is on a definition span
        if found_def.is_none() {
            // Check if any usage points to a definition that contains the cursor
            for def_span in file_state.analysis.references.values() {
                if def_span.contains_offset(offset) {
                    found_def = Some(*def_span);
                    break;
                }
            }
        }

        found_def?
    };

    // Now collect all usages that point to this definition
    let mut locations = Vec::new();

    // Optionally include the declaration itself
    if include_declaration
        && let Some(range) = file_state
            .analysis
            .diagnostics
            .span_to_range(def_span)
            .map(source_range_to_lsp_range)
    {
        locations.push(Location {
            uri: uri.clone(),
            range,
        });
    }

    // Find all usages
    for (usage_span, usage_def_span) in &file_state.analysis.references {
        if *usage_def_span == def_span
            && let Some(range) = file_state
                .analysis
                .diagnostics
                .span_to_range(*usage_span)
                .map(source_range_to_lsp_range)
        {
            locations.push(Location {
                uri: uri.clone(),
                range,
            });
        }
    }

    if locations.is_empty() {
        None
    } else {
        Some(locations)
    }
}

/// Check if a definition span corresponds to a function definition.
/// Returns true if the span is preceded by `fn `.
fn is_function_definition(text: &str, def_start: usize) -> bool {
    // Look backwards from the definition start, skipping whitespace
    let bytes = text.as_bytes();
    let mut pos = def_start;

    // Skip whitespace backwards
    while pos > 0 && (bytes[pos - 1] as char).is_whitespace() {
        pos -= 1;
    }

    // Check for "fn" keyword (need at least 2 chars)
    if pos >= 2 {
        let potential_fn = &text[pos - 2..pos];
        if potential_fn == "fn" {
            // Make sure it's not part of a larger identifier
            if pos == 2 || !bytes[pos - 3].is_ascii_alphanumeric() && bytes[pos - 3] != b'_' {
                return true;
            }
        }
    }

    false
}

pub fn prepare_rename(
    file_state: &mut FileState,
    position: Position,
) -> Option<PrepareRenameResponse> {
    let offset = position_to_offset(&file_state.text, position)?;

    // Check if cursor is on a usage span
    for (usage_span, def_span) in &file_state.analysis.references {
        if usage_span.contains_offset(offset) {
            // Refuse to rename if the definition is a function
            if is_function_definition(&file_state.text, def_span.start()) {
                return None;
            }

            let start = usage_span.start();
            let end = usage_span.end();
            let name = file_state.text.get(start..end)?;

            let range = file_state
                .analysis
                .diagnostics
                .span_to_range(*usage_span)
                .map(source_range_to_lsp_range)?;

            return Some(PrepareRenameResponse::RangeWithPlaceholder {
                range,
                placeholder: name.to_string(),
            });
        }
    }

    // Check if cursor is on a definition span
    for def_span in file_state.analysis.references.values() {
        if def_span.contains_offset(offset) {
            // Refuse to rename if this is a function definition
            if is_function_definition(&file_state.text, def_span.start()) {
                return None;
            }

            let start = def_span.start();
            let end = def_span.end();
            let name = file_state.text.get(start..end)?;

            let range = file_state
                .analysis
                .diagnostics
                .span_to_range(*def_span)
                .map(source_range_to_lsp_range)?;

            return Some(PrepareRenameResponse::RangeWithPlaceholder {
                range,
                placeholder: name.to_string(),
            });
        }
    }

    None
}

pub fn find_rename(
    file_state: &mut FileState,
    uri: &Url,
    position: Position,
    new_name: String,
) -> Option<WorkspaceEdit> {
    let offset = position_to_offset(&file_state.text, position)?;

    // Find the definition span for the symbol at cursor
    let def_span = {
        // Check if cursor is on a usage span
        let mut found_def = None;
        for (usage_span, def_span) in &file_state.analysis.references {
            if usage_span.contains_offset(offset) {
                found_def = Some(*def_span);
                break;
            }
        }

        // If not found as usage, check if cursor is on a definition span
        if found_def.is_none() {
            for def_span in file_state.analysis.references.values() {
                if def_span.contains_offset(offset) {
                    found_def = Some(*def_span);
                    break;
                }
            }
        }

        found_def?
    };

    // Collect all text edits
    let mut edits = Vec::new();

    // Add edit for the definition
    if let Some(range) = file_state
        .analysis
        .diagnostics
        .span_to_range(def_span)
        .map(source_range_to_lsp_range)
    {
        edits.push(TextEdit {
            range,
            new_text: new_name.clone(),
        });
    }

    // Add edits for all usages (skip the definition itself since we already added it)
    for (usage_span, usage_def_span) in &file_state.analysis.references {
        if *usage_def_span == def_span
            && *usage_span != def_span
            && let Some(range) = file_state
                .analysis
                .diagnostics
                .span_to_range(*usage_span)
                .map(source_range_to_lsp_range)
        {
            edits.push(TextEdit {
                range,
                new_text: new_name.clone(),
            });
        }
    }

    if edits.is_empty() {
        return None;
    }

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    })
}
