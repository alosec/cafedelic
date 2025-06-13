#!/bin/bash
# Install cafe command to user PATH

set -euo pipefail

INSTALL_DIR="$HOME/.local/bin"
CAFEDELIC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Installing Cafedelic CLI..."

# Create install directory if it doesn't exist
mkdir -p "$INSTALL_DIR"

# Create symlink to cafe command
if ln -sf "$CAFEDELIC_DIR/cli/cafe" "$INSTALL_DIR/cafe"; then
    echo "✓ cafe command linked to $INSTALL_DIR/cafe"
else
    echo "✗ Failed to create symlink"
    exit 1
fi

# Check if ~/.local/bin is in PATH
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    echo ""
    echo "⚠️  Warning: $HOME/.local/bin is not in your PATH"
    echo "   Add this line to your ~/.bashrc or ~/.zshrc:"
    echo "   export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo ""
fi

echo "✓ cafe command installed successfully!"
echo ""
echo "Usage:"
echo "  cafe init          # Check system readiness"
echo "  cafe deploy        # Create IDE layout"
echo "  cafe --help        # Show help"
