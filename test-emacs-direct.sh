#!/bin/bash
# Manual test to check if emacs integration is working

echo "=== Testing Emacs Integration ==="
echo ""

# Check if scripts exist
SCRIPT_DIR="/home/alex/code/cafedelic/scripts/emacs"
echo "Checking script directory: $SCRIPT_DIR"
if [ -d "$SCRIPT_DIR" ]; then
    echo "✅ Script directory exists"
    ls -la "$SCRIPT_DIR/open-*-v2.sh"
else
    echo "❌ Script directory not found!"
fi

echo ""
echo "Testing file open script directly..."
if [ -f "$SCRIPT_DIR/open-claude-file-v2.sh" ]; then
    echo "Running: $SCRIPT_DIR/open-claude-file-v2.sh /home/alex/code/cafedelic/README.md"
    "$SCRIPT_DIR/open-claude-file-v2.sh" "/home/alex/code/cafedelic/README.md"
else
    echo "❌ Script not found!"
fi

echo ""
echo "=== End Test ==="
