import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration("tapir");
  let serverPath = config.get<string>("serverPath");

  if (!serverPath) {
    // Default: assume tapir-lsp is in PATH or use a workspace-relative path
    serverPath = "tapir-lsp";
  }

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "tapir" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.tapir"),
    },
  };

  client = new LanguageClient(
    "tapir-lsp",
    "Tapir Language Server",
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
