#!/bin/bash
# Test the pane management scripts

echo "Testing Cafedelic Pane Management"
echo "================================="

# Get current session and window
SESSION=$(tmux display-message -p '#S')
WINDOW=$(tmux display-message -p '#I')

echo "Current session: $SESSION"
echo "Current window: $WINDOW"
echo ""

# Test 1: Assign names to panes
echo "1. Testing pane naming..."
./scripts/pane-management/assign-name.sh "$SESSION" "$WINDOW" 0 "test-pane-1"
./scripts/pane-management/assign-name.sh "$SESSION" "$WINDOW" 1 "test-pane-2"

# Test 2: List named panes
echo ""
echo "2. Listing named panes..."
./scripts/pane-management/list-named-panes.sh

# Test 3: Send text to a pane
echo ""
echo "3. Sending text to test-pane-1..."
./scripts/pane-management/send-to-pane.sh "test-pane-1" "Hello from cafedelic!"

# Test 4: Send special key
echo ""
echo "4. Sending Enter key to test-pane-1..."
./scripts/pane-management/send-special-key.sh "test-pane-1" "enter"

# Test 5: Read from pane
echo ""
echo "5. Reading last 5 lines from test-pane-1..."
./scripts/pane-management/read-pane.sh "test-pane-1" 5

# Test 6: Configure routing
echo ""
echo "6. Setting up routing..."
./scripts/routing/set-output-destination.sh "files" "test-pane-1"
./scripts/routing/set-output-destination.sh "activity" "test-pane-2"

# Test 7: Show routing config
echo ""
echo "7. Current routing configuration..."
./scripts/routing/get-routing-config.sh

echo ""
echo "Testing complete!"
