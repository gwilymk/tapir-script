# Install the tapir-lsp binary to ~/.cargo/bin
install-lsp:
    cargo install --path tapir-lsp

# Build and install the VS Code extension
install-vscode: install-lsp
    cd tapir-vscode && npm run bundle
    cd tapir-vscode && npx @vscode/vsce package
    code --install-extension tapir-vscode/tapir-script-0.0.1.vsix --force

# Install both lsp and vscode extension
install: install-vscode
