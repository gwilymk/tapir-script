import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export async function activate(
  _context: vscode.ExtensionContext,
): Promise<void> {
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
    clientOptions,
  );

  await client.start();
}

export async function deactivate(): Promise<void> {
  if (!client) {
    return undefined;
  }
  return await client.stop();
}
