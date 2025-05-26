#!/bin/bash
# Test script for output routing functionality

echo "========================================="
echo "Cafedelic Output Routing Test"
echo "========================================="
echo ""

# Check if server is running
echo "Checking if Cafedelic server is running..."
if pgrep -f "cafedelic.*index.ts" > /dev/null; then
    echo "✅ Server is running"
else
    echo "❌ Server is not running. Please start it first."
    exit 1
fi

# Test the output router service
echo ""
echo "Testing Output Router Service..."
echo ""

# Create a simple test by opening a file
TEST_FILE="/tmp/cafedelic-test-file.txt"
echo "Creating test file: $TEST_FILE"
echo "This is a test file for Cafedelic output routing" > "$TEST_FILE"

# Use the new v2 script
SCRIPT_PATH="$(dirname "$0")/scripts/emacs/open-claude-file-v2.sh"
echo ""
echo "Opening file with new script (no hard-coded routing)..."
"$SCRIPT_PATH" "$TEST_FILE"

echo ""
echo "========================================="
echo "Test Complete!"
echo ""
echo "Check tmux pane 9:0.2 to see if the output was routed correctly."
echo ""
echo "To assign a different pane for output:"
echo "1. Find your target pane ID: tmux list-panes -a -F '#{pane_id} #{session_name}:#{window_index}.#{pane_index}'"
echo "2. Use Claude: 'assign_pane_role' with paneId and role='editor-output'"
echo ""
echo "To check current routing:"
echo "Use Claude: 'get_output_routing' to see all assignments"
echo "========================================="
