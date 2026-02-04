use std::collections::HashMap;
use std::error::Error;
use std::time::Instant;

use compiler::Severity;
use compiler::{AnalysisResult, CompileSettings};
use lsp_server::{Connection, Message, Notification};
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, PublishDiagnosticsParams, Url,
    notification::{Notification as _, PublishDiagnostics},
};

use crate::state::FileState;
use crate::util::source_range_to_lsp_range;

pub fn analyse_and_publish(
    connection: &Connection,
    uri: Url,
    text: String,
    files: &mut HashMap<Url, FileState>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let filename = uri.path();

    // When editing the prelude itself, disable prelude loading
    // so the file is treated as the prelude (allowing builtin declarations)
    let is_prelude = filename.ends_with("prelude.tapir");

    let settings = CompileSettings {
        available_fields: None,
        enable_optimisations: false,
        enable_prelude: !is_prelude,
        // LSP doesn't know about Rust attributes, so be permissive
        has_event_type: true,
    };

    let start = Instant::now();
    let mut analysis = compiler::analyse(filename, &text, &settings);
    let elapsed = start.elapsed();

    eprintln!("[tapir-lsp] Analysed {filename} in {elapsed:?}");

    let diagnostics = convert_diagnostics(uri.clone(), &mut analysis);

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

fn convert_diagnostics(uri: Url, result: &mut AnalysisResult) -> Vec<Diagnostic> {
    // Collect diagnostic info first to avoid borrow conflicts
    let diag_info: Vec<_> = result
        .diagnostics
        .iter()
        .map(|diag| {
            (
                diag.primary_span,
                diag.severity,
                diag.kind.code().to_string(),
                diag.message(),
                diag.labels.clone(),
            )
        })
        .collect();

    diag_info
        .into_iter()
        .filter_map(|(span, severity, code, message, labels)| {
            let range = result
                .diagnostics
                .span_to_range(span)
                .map(source_range_to_lsp_range)?;

            let lsp_severity = match severity {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
            };

            Some(Diagnostic {
                range,
                severity: Some(lsp_severity),
                code: Some(lsp_types::NumberOrString::String(code)),
                source: Some("tapir".to_string()),
                message,
                related_information: if labels.is_empty() {
                    None
                } else {
                    Some(
                        labels
                            .iter()
                            .filter_map(|(span, info)| {
                                Some(DiagnosticRelatedInformation {
                                    location: lsp_types::Location {
                                        range: result
                                            .diagnostics
                                            .span_to_range(*span)
                                            .map(source_range_to_lsp_range)?,
                                        uri: uri.clone(),
                                    },
                                    message: info.render(),
                                })
                            })
                            .collect(),
                    )
                },
                tags: None,
                code_description: None,
                data: None,
            })
        })
        .collect()
}
