#!/bin/bash
# Test Cafedelic V2 Property-Based Pane Management

echo "Testing Cafedelic V2 Pane Management"
echo "===================================="

# Get current session and window
SESSION=$(tmux display-message -p '#S')
WINDOW=$(tmux display-message -p '#I')

echo "Current session: $SESSION"
echo "Current window: $WINDOW"
echo ""

# Test 1: Assign properties to panes (V2 approach)
echo "1. Testing V2 property assignment..."
./scripts/pane-properties/assign-properties.sh "$SESSION" "$WINDOW" 0 --name "test-editor" --source "user" --role "editor"
./scripts/pane-properties/assign-properties.sh "$SESSION" "$WINDOW" 1 --name "test-terminal" --source "user" --role "terminal"

# Test 2: List panes by properties
echo ""
echo "2. Listing panes by properties..."
echo "   All user panes:"
./scripts/pane-properties/list-panes-by-properties.sh --source "user"
echo "   All editor panes:"
./scripts/pane-properties/list-panes-by-properties.sh --role "editor"

# Test 3: Find specific pane by source and role
echo ""
echo "3. Finding user editor pane..."
./scripts/pane-properties/find-pane-by-source-and-role.sh "user" "editor"

# Test 4: Send text to a pane
echo ""
echo "4. Sending text to test-editor..."
./scripts/pane-management/send-keys-to-pane.sh "test-editor" "echo 'Hello from Cafedelic V2!'"

# Test 5: Send special key
echo ""
echo "5. Sending Enter key to test-editor..."
./scripts/pane-management/send-special-key.sh "test-editor" "enter"

# Test 6: Capture pane content with properties
echo ""
echo "6. Reading last 5 lines from user editor..."
./scripts/pane-properties/capture-pane-with-properties.sh --source "user" --role "editor" --last 5

# Test 7: Test best pane discovery
echo ""
echo "7. Finding best pane for editor role..."
./scripts/pane-properties/find-best-pane-for-role.sh "editor"

# Test 8: Advanced capture with grep
echo ""
echo "8. Searching for 'Hello' in editor pane..."
./scripts/pane-properties/capture-pane-with-properties.sh --name "test-editor" --grep "Hello" --grep-context 1

echo ""
echo "V2 Testing complete!"
echo ""
echo "Key differences from V1:"
echo "- Uses source + role properties instead of just names"
echo "- Property-based pane discovery with fallbacks"
echo "- Advanced capture options (grep, context, ranges)"
echo "- Multi-dimensional pane organization"
