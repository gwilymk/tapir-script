use std::{collections::HashMap, error::Error};

use compiler::{AnalysisResult, CompileSettings, SourceRange};
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, Location, MarkupContent, MarkupKind, OneOf,
    Position, PublishDiagnosticsParams, Range, ReferenceParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    notification::{DidChangeTextDocument, DidOpenTextDocument, Notification as _, PublishDiagnostics},
    request::{GotoDefinition, HoverRequest, References, Request as _},
};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("[tapir-lsp] Starting...");

    let (connection, io_thread) = Connection::stdio();

    let caps = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        references_provider: Some(OneOf::Left(true)),
        ..Default::default()
    };

    let server_capabilities = serde_json::to_value(caps)?;
    let _init_params: InitializeParams =
        serde_json::from_value(connection.initialize(server_capabilities)?)?;

    eprintln!("[tapir-lsp] Initialized");

    main_loop(connection)?;
    io_thread.join().unwrap();

    eprintln!("[tapir-lsp] Shutting down");
    Ok(())
}

struct FileState {
    text: String,
    analysis: AnalysisResult,
}

fn main_loop(connection: Connection) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut files: HashMap<Url, FileState> = HashMap::new();

    for msg in &connection.receiver {
        match msg {
            Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    break;
                }
                handle_request(&connection, request, &mut files)?;
            }
            Message::Response(_response) => {
                // We don't send requests, so we shouldn't get responses
            }
            Message::Notification(notification) => {
                handle_notification(&connection, notification, &mut files)?;
            }
        }
    }

    Ok(())
}

fn handle_request(
    connection: &Connection,
    request: Request,
    files: &mut HashMap<Url, FileState>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    match request.method.as_str() {
        GotoDefinition::METHOD => {
            let (id, params): (_, GotoDefinitionParams) =
                request.extract(GotoDefinition::METHOD)?;

            let uri = params.text_document_position_params.text_document.uri;
            let position = params.text_document_position_params.position;

            let response = if let Some(file_state) = files.get_mut(&uri) {
                find_definition(file_state, &uri, position)
            } else {
                None
            };

            let result = serde_json::to_value(response)?;
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, result)))?;
        }
        HoverRequest::METHOD => {
            let (id, params): (_, HoverParams) = request.extract(HoverRequest::METHOD)?;

            let uri = params.text_document_position_params.text_document.uri;
            let position = params.text_document_position_params.position;

            let response = if let Some(file_state) = files.get_mut(&uri) {
                find_hover(file_state, position)
            } else {
                None
            };

            let result = serde_json::to_value(response)?;
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, result)))?;
        }
        References::METHOD => {
            let (id, params): (_, ReferenceParams) = request.extract(References::METHOD)?;

            let uri = params.text_document_position.text_document.uri;
            let position = params.text_document_position.position;
            let include_declaration = params.context.include_declaration;

            let response = if let Some(file_state) = files.get_mut(&uri) {
                find_references(file_state, &uri, position, include_declaration)
            } else {
                None
            };

            let result = serde_json::to_value(response)?;
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, result)))?;
        }
        _ => {
            let response = Response::new_err(
                request.id,
                lsp_server::ErrorCode::MethodNotFound as i32,
                format!("Method not found: {}", request.method),
            );
            connection.sender.send(Message::Response(response))?;
        }
    }

    Ok(())
}

fn find_definition(
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

fn find_hover(file_state: &mut FileState, position: Position) -> Option<Hover> {
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

            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```tapir\n{}\n```", info.description),
                }),
                range,
            });
        }
    }

    None
}

fn find_references(
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
            for (_usage_span, def_span) in &file_state.analysis.references {
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
    if include_declaration {
        if let Some(range) = file_state
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
    }

    // Find all usages
    for (usage_span, usage_def_span) in &file_state.analysis.references {
        if *usage_def_span == def_span {
            if let Some(range) = file_state
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
    }

    if locations.is_empty() {
        None
    } else {
        Some(locations)
    }
}

fn position_to_offset(text: &str, position: Position) -> Option<usize> {
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

fn handle_notification(
    connection: &Connection,
    notification: Notification,
    files: &mut HashMap<Url, FileState>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    match notification.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(notification.params)?;
            let uri = params.text_document.uri;
            let text = params.text_document.text;

            analyse_and_publish(connection, uri, text, files)?;
        }
        DidChangeTextDocument::METHOD => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(notification.params)?;
            let uri = params.text_document.uri;

            // We use full sync, so there's exactly one change with the full text
            if let Some(change) = params.content_changes.into_iter().next() {
                analyse_and_publish(connection, uri, change.text, files)?;
            }
        }
        _ => {
            // Ignore other notifications
        }
    }

    Ok(())
}

fn analyse_and_publish(
    connection: &Connection,
    uri: Url,
    text: String,
    files: &mut HashMap<Url, FileState>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let filename = uri.path();

    let settings = CompileSettings {
        available_fields: None,
        enable_optimisations: false,
    };

    let mut analysis = compiler::analyse(filename, &text, &settings);
    let diagnostics = convert_diagnostics(&mut analysis);

    files.insert(uri.clone(), FileState { text, analysis });

    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };

    let notification = Notification::new(PublishDiagnostics::METHOD.to_string(), params);
    connection
        .sender
        .send(Message::Notification(notification))?;

    Ok(())
}

fn convert_diagnostics(result: &mut AnalysisResult) -> Vec<Diagnostic> {
    // Collect diagnostic info first to avoid borrow conflicts
    let diag_info: Vec<_> = result
        .diagnostics
        .iter()
        .map(|diag| (diag.primary_span, diag.kind.code().to_string(), diag.message()))
        .collect();

    diag_info
        .into_iter()
        .filter_map(|(span, code, message)| {
            let range = result
                .diagnostics
                .span_to_range(span)
                .map(source_range_to_lsp_range)?;

            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(lsp_types::NumberOrString::String(code)),
                source: Some("tapir".to_string()),
                message,
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            })
        })
        .collect()
}

fn source_range_to_lsp_range(range: SourceRange) -> Range {
    Range {
        start: Position::new(range.start.line as u32, range.start.column as u32),
        end: Position::new(range.end.line as u32, range.end.column as u32),
    }
}
