use std::collections::HashMap;
use std::error::Error;

use lsp_server::{Connection, Message, Notification, Response};
use lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams, HoverParams,
    InlayHintParams, ReferenceParams, RenameParams, SignatureHelpParams,
    TextDocumentPositionParams, Url,
    notification::{DidChangeTextDocument, DidOpenTextDocument, Notification as _},
    request::{
        GotoDefinition, HoverRequest, InlayHintRequest, PrepareRenameRequest, References, Rename,
        Request, SignatureHelpRequest,
    },
};

use crate::diagnostics::analyse_and_publish;
use crate::features::{
    find_definition, find_hover, find_references, find_rename, find_signature_help,
    get_inlay_hints, prepare_rename,
};
use crate::state::FileState;

pub fn handle_request(
    connection: &Connection,
    request: lsp_server::Request,
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
        SignatureHelpRequest::METHOD => {
            let (id, params): (_, SignatureHelpParams) =
                request.extract(SignatureHelpRequest::METHOD)?;

            let uri = params.text_document_position_params.text_document.uri;
            let position = params.text_document_position_params.position;

            let response = if let Some(file_state) = files.get(&uri) {
                find_signature_help(file_state, position)
            } else {
                None
            };

            let result = serde_json::to_value(response)?;
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, result)))?;
        }
        InlayHintRequest::METHOD => {
            let (id, params): (_, InlayHintParams) = request.extract(InlayHintRequest::METHOD)?;

            let uri = params.text_document.uri;

            let response = files.get(&uri).map(get_inlay_hints);

            let result = serde_json::to_value(response)?;
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, result)))?;
        }
        PrepareRenameRequest::METHOD => {
            let (id, params): (_, TextDocumentPositionParams) =
                request.extract(PrepareRenameRequest::METHOD)?;

            let uri = params.text_document.uri;
            let position = params.position;

            let response = files
                .get_mut(&uri)
                .and_then(|state| prepare_rename(state, position));

            let result = serde_json::to_value(response)?;
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, result)))?;
        }
        Rename::METHOD => {
            let (id, params): (_, RenameParams) = request.extract(Rename::METHOD)?;

            let uri = params.text_document_position.text_document.uri;
            let position = params.text_document_position.position;
            let new_name = params.new_name;

            let response = files
                .get_mut(&uri)
                .and_then(|state| find_rename(state, &uri, position, new_name));

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

pub fn handle_notification(
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
