#!/usr/bin/env bash

# setup-carousel-demo.sh - Set up the carousel demo environment

set -euo pipefail

echo "=== Setting up Carousel Demo ==="

# Check if we're in tmux
if [[ -z "${TMUX:-}" ]]; then
    echo "Error: This must be run inside a tmux session"
    echo "Run: tmux new-session -s carousel-demo"
    exit 1
fi

# Get current window
CURRENT_WINDOW=$(tmux display-message -p '#{session_name}:#{window_index}')

# Reset to single pane first
echo "Resetting window to single pane..."
./scripts/tmux-clear.sh "${CURRENT_WINDOW}" reset-layout

# Create horizontal split (top pane small, bottom pane large)
echo "Creating horizontal split..."
tmux split-window -v -p 95  # Bottom pane gets 95%

# Get the pane IDs
PANE0=$(tmux list-panes -F '#{pane_index}' | head -1)
PANE1=$(tmux list-panes -F '#{pane_index}' | tail -1)

# Resize top pane to be minimal (for control)
tmux resize-pane -t "${CURRENT_WINDOW}.${PANE0}" -y 5

# Select the top pane (control pane)
tmux select-pane -t "${CURRENT_WINDOW}.${PANE0}"

echo ""
echo "=== Demo Environment Ready ==="
echo "Pane 0 (top): Control pane - Run commands here"
echo "Pane 1 (bottom): Demo pane - Tmex layouts will appear here"
echo ""
echo "To start the carousel, run:"
echo "  ./scripts/tmex-pane-carousel.sh ${CURRENT_WINDOW}.${PANE1} 2"
echo ""
echo "To manually test layouts:"
echo "  ./scripts/tmex-in-pane.sh 111 ${CURRENT_WINDOW}.${PANE1}"
echo "  ./scripts/tmux-clear.sh . reset-keep-two"
echo ""