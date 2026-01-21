#![warn(clippy::all)]
mod diagnostics;
mod features;
mod handlers;
mod state;
mod util;

use std::{collections::HashMap, error::Error};

use lsp_server::{Connection, Message};
use lsp_types::{
    HoverProviderCapability, InitializeParams, OneOf, ServerCapabilities, SignatureHelpOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

use handlers::{handle_notification, handle_request};
use state::FileState;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("[tapir-lsp] Starting...");

    let (connection, io_thread) = Connection::stdio();

    let caps = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        references_provider: Some(OneOf::Left(true)),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: Default::default(),
        }),
        inlay_hint_provider: Some(OneOf::Left(true)),
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
