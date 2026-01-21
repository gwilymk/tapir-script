use std::{collections::HashMap, error::Error};

use compiler::{CompileSettings, SourceRange};
use lsp_server::{Connection, Message, Notification, Response};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, Notification as _, PublishDiagnostics,
    },
};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("[tapir-lsp] Starting...");

    let (connection, io_thread) = Connection::stdio();

    let caps = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
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

fn main_loop(connection: Connection) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut files: HashMap<Url, String> = HashMap::new();

    for msg in &connection.receiver {
        match msg {
            Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    break;
                }
                // We don't handle any requests yet, just shutdown
                let response = Response::new_err(
                    request.id,
                    lsp_server::ErrorCode::MethodNotFound as i32,
                    format!("Method not found: {}", request.method),
                );
                connection.sender.send(Message::Response(response))?;
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

fn handle_notification(
    connection: &Connection,
    notification: Notification,
    files: &mut HashMap<Url, String>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    match notification.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(notification.params)?;
            let uri = params.text_document.uri;
            let text = params.text_document.text;

            files.insert(uri.clone(), text.clone());
            publish_diagnostics(connection, uri, &text)?;
        }
        DidChangeTextDocument::METHOD => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(notification.params)?;
            let uri = params.text_document.uri;

            // We use full sync, so there's exactly one change with the full text
            if let Some(change) = params.content_changes.into_iter().next() {
                files.insert(uri.clone(), change.text.clone());
                publish_diagnostics(connection, uri, &change.text)?;
            }
        }
        _ => {
            // Ignore other notifications
        }
    }

    Ok(())
}

fn publish_diagnostics(
    connection: &Connection,
    uri: Url,
    text: &str,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let filename = uri.path();

    let settings = CompileSettings {
        available_fields: None,
        enable_optimisations: false,
    };

    let mut result = compiler::analyse(filename, text, &settings);

    let diagnostics = convert_diagnostics(&mut result.diagnostics);

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

fn convert_diagnostics(diagnostics: &mut compiler::Diagnostics) -> Vec<Diagnostic> {
    // Collect diagnostic info first to avoid borrow conflicts
    let diag_info: Vec<_> = diagnostics
        .iter()
        .map(|diag| {
            (
                diag.primary_span,
                diag.kind.code().to_string(),
                diag.message(),
            )
        })
        .collect();

    diag_info
        .into_iter()
        .map(|(span, code, message)| {
            let range = diagnostics
                .span_to_range(span)
                .map(source_range_to_lsp_range)
                .unwrap_or_default();

            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(lsp_types::NumberOrString::String(code)),
                source: Some("tapir".to_string()),
                message,
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            }
        })
        .collect()
}

fn source_range_to_lsp_range(range: SourceRange) -> Range {
    Range {
        start: Position::new(range.start.line as u32, range.start.column as u32),
        end: Position::new(range.end.line as u32, range.end.column as u32),
    }
}
