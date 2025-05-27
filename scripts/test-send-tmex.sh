#!/usr/bin/env bash

# test-send-tmex.sh - Test the send-tmex.sh script

set -euo pipefail

echo "=== Testing send-tmex.sh ==="
echo ""

# Check if we're in tmux
if [[ -z "${TMUX:-}" ]]; then
    echo "This test must be run inside a tmux session"
    echo "Run: tmux new-session -s test"
    exit 1
fi

# Get current pane
CURRENT_PANE=$(tmux display-message -p '#P')
echo "Current pane: %${CURRENT_PANE}"

# Create a horizontal split
echo "Creating horizontal split for demo..."
tmux split-window -h -p 80
DEMO_PANE=$(tmux display-message -p '#P')
echo "Demo pane: %${DEMO_PANE}"

# Select original pane
tmux select-pane -t "%${CURRENT_PANE}"

echo ""
echo "Ready to test! Try these commands:"
echo ""
echo "  ./scripts/send-tmex.sh 111 %${DEMO_PANE}     # Three columns"
echo "  ./scripts/send-tmex.sh 1234 %${DEMO_PANE}    # Progressive columns"
echo "  ./scripts/send-tmex.sh 12[34]5 %${DEMO_PANE} # Complex layout"
echo ""
echo "Or run the carousel:"
echo "  ./scripts/tmex-carousel.sh %${DEMO_PANE} 2"
echo ""