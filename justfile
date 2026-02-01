# Install the tapirc binary to ~/.cargo/bin
install-cli:
    cargo install --path tapir-cli

# Build and install the VS Code extension
install-vscode: install-cli
    cd tapir-vscode && npm run bundle
    cd tapir-vscode && npx @vscode/vsce package
    code --install-extension tapir-vscode/tapir-script-0.0.1.vsix --force

# Install both cli and vscode extension
install: install-vscode
