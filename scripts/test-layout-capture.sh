#!/usr/bin/env bash

# test-layout-capture.sh - Test the layout capture functionality

set -euo pipefail

echo "=== Testing Layout Capture ==="
echo ""
echo "1. Capture current window to console:"
./scripts/capture-tmux-layout.sh
echo ""
echo "Press Enter to continue..."
read

echo "2. Create a complex layout and capture it:"
tmux split-window -h
tmux split-window -v
echo "Created 3-pane layout"
sleep 1

./scripts/capture-tmux-layout.sh
echo ""
echo "Press Enter to reset..."
read

./scripts/tmux-clear.sh . reset-layout
echo "Reset complete"