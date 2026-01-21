# Tapir Script VS Code Extension

Language support for Tapir Script, including syntax highlighting and error diagnostics.

## Building

### 1. Build the LSP server

```bash
cd /path/to/tapir-game-tools
cargo build --release -p tapir-lsp
```

### 2. Install extension dependencies and compile

```bash
cd crates/tapir-script/tapir-vscode
npm install
npm run compile
```

## Installing

### Option A: Development mode (easiest for testing)

From the `tapir-vscode` directory:

```bash
code --extensionDevelopmentPath=$(pwd)
```

### Option B: Install as VSIX

```bash
# Install vsce if you don't have it
npm install -g @vscode/vsce

# Package the extension
vsce package

# Install the generated .vsix file
code --install-extension tapir-script-0.0.1.vsix
```

## Configuration

If `tapir-lsp` is not in your PATH, set the path in VS Code settings:

```json
{
  "tapir.serverPath": "/path/to/tapir-game-tools/target/release/tapir-lsp"
}
```

## Features

- Syntax highlighting for `.tapir` files
- Real-time error diagnostics from the Tapir compiler
