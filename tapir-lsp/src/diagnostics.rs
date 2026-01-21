use std::collections::HashMap;
use std::error::Error;
use std::time::Instant;

use compiler::{AnalysisResult, CompileSettings};
use lsp_server::{Connection, Message, Notification};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams, Url,
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

    let settings = CompileSettings {
        available_fields: None,
        enable_optimisations: false,
    };

    let start = Instant::now();
    let mut analysis = compiler::analyse(filename, &text, &settings);
    let elapsed = start.elapsed();

    eprintln!("[tapir-lsp] Analysed {filename} in {elapsed:?}");

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
