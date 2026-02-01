import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel | undefined;

function getServerOptions(): ServerOptions {
  const config = vscode.workspace.getConfiguration("tapir");
  let serverPath = config.get<string>("serverPath");

  if (!serverPath) {
    // Default: assume tapirc is in PATH
    serverPath = "tapirc";
  }

  return {
    command: serverPath,
    args: ["lsp"],
  };
}

function getClientOptions(): LanguageClientOptions {
  return {
    documentSelector: [{ scheme: "file", language: "tapir" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.tapir"),
    },
    outputChannel,
  };
}

async function startClient(): Promise<void> {
  client = new LanguageClient(
    "tapir-lsp",
    "Tapir Language Server",
    getServerOptions(),
    getClientOptions(),
  );

  await client.start();
}

async function restartServer(): Promise<void> {
  if (client) {
    await client.dispose();
    client = undefined;
  }
  await startClient();
  vscode.window.showInformationMessage("Tapir Language Server restarted");
}

export async function activate(
  context: vscode.ExtensionContext,
): Promise<void> {
  outputChannel = vscode.window.createOutputChannel("Tapir Language Server");
  context.subscriptions.push(outputChannel);

  context.subscriptions.push(
    vscode.commands.registerCommand("tapir.restartServer", restartServer),
  );

  await startClient();
}

export async function deactivate(): Promise<void> {
  if (!client) {
    return;
  }
  await client.dispose();
}
