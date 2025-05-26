#!/bin/bash

# Enhanced Tmux Window Visualizer with ASCII art

SESSION="${1:-0}"
WINDOW="${2:-0}"

echo "╔═══════════════════════════════════════════════════╗"
echo "║           Tmux Window Layout Visualizer           ║"
echo "╚═══════════════════════════════════════════════════╝"

# Check if session exists
if ! tmux has-session -t "$SESSION" 2>/dev/null; then
    echo "❌ Session '$SESSION' not found!"
    echo ""
    echo "Available sessions:"
    tmux list-sessions 2>/dev/null || echo "  No tmux sessions running"
    exit 1
fi

echo ""

# Get window info
WINDOW_WIDTH=$(tmux display-message -t "$SESSION:$WINDOW" -p '#{window_width}')
WINDOW_HEIGHT=$(tmux display-message -t "$SESSION:$WINDOW" -p '#{window_height}')
echo "Session: $SESSION | Window: $WINDOW | Size: ${WINDOW_WIDTH}x${WINDOW_HEIGHT}"
echo ""

# List panes with details
echo "Panes:"
tmux list-panes -t "$SESSION:$WINDOW" -F "  [#{pane_index}] #{pane_width}x#{pane_height} @ (#{pane_left},#{pane_top}) - #{pane_current_command} #{?pane_active,[ACTIVE],}"

echo ""
echo "Layout String:"
tmux list-windows -t "$SESSION" -F "  #{window_layout}" | head -1

# Create ASCII representation
echo ""
echo "Visual Layout (simplified):"
echo ""

# Get pane information for drawing
PANES=$(tmux list-panes -t "$SESSION:$WINDOW" -F "#{pane_index}:#{pane_left}:#{pane_top}:#{pane_width}:#{pane_height}:#{pane_current_command}")

# Scale factor for drawing (terminal columns to ASCII)
SCALE_X=80
SCALE_Y=30

# Draw a simple box representation
for pane in $PANES; do
    IFS=':' read -r idx left top width height cmd <<< "$pane"
    
    # Calculate scaled dimensions
    x1=$((left * SCALE_X / WINDOW_WIDTH))
    y1=$((top * SCALE_Y / WINDOW_HEIGHT))
    x2=$(((left + width) * SCALE_X / WINDOW_WIDTH))
    y2=$(((top + height) * SCALE_Y / WINDOW_HEIGHT))
    
    # Show pane info
    echo "Pane $idx: $cmd"
    echo "  Position: ($x1,$y1) to ($x2,$y2)"
    echo "  Size: $((x2-x1))x$((y2-y1)) chars"
done

# Get active pane
ACTIVE_PANE=$(tmux display-message -t "$SESSION:$WINDOW" -p '#{pane_index}')
echo ""
echo "Active pane: $ACTIVE_PANE"
