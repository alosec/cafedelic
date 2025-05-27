#!/usr/bin/env bash

# capture-tmux-layout.sh - Capture mathematical representation of tmux layout
# Usage: ./capture-tmux-layout.sh [session:window] [output-mode]
# output-mode: "console" (default), "pane:TARGET", or "both:TARGET"

set -euo pipefail

TARGET="${1:-.}"  # Default to current window
OUTPUT_MODE="${2:-console}"  # Default to console output

# Parse session and window
if [[ "$TARGET" == "." ]]; then
    SESSION=$(tmux display-message -p '#{session_name}')
    WINDOW=$(tmux display-message -p '#{window_index}')
else
    SESSION="${TARGET%:*}"
    WINDOW="${TARGET#*:}"
fi

# Function to generate layout report
generate_layout() {
    local timestamp=$(date '+%H:%M:%S')
    
    echo "=== Layout Snapshot @ $timestamp ==="
    echo "Session: $SESSION | Window: $WINDOW"
    
    # Get window dimensions
    local window_width=$(tmux display-message -t "$SESSION:$WINDOW" -p '#{window_width}')
    local window_height=$(tmux display-message -t "$SESSION:$WINDOW" -p '#{window_height}')
    echo "Window Size: ${window_width}x${window_height}"
    
    # Get tmux layout string
    echo ""
    echo "Layout String:"
    tmux list-windows -t "$SESSION:$WINDOW" -F "  #{window_layout}"
    
    # List panes with mathematical representation
    echo ""
    echo "Panes (Mathematical Representation):"
    echo "ID | Position (x,y) | Size (w×h) | Command"
    echo "---|----------------|-------------|--------"
    
    tmux list-panes -t "$SESSION:$WINDOW" -F "#{pane_index}|#{pane_left},#{pane_top}|#{pane_width}×#{pane_height}|#{pane_current_command}" | \
    while IFS='|' read -r id pos size cmd; do
        printf "%-2s | %-14s | %-11s | %s\n" "$id" "($pos)" "$size" "$cmd"
    done
    
    # Calculate total panes
    local pane_count=$(tmux list-panes -t "$SESSION:$WINDOW" | wc -l)
    echo ""
    echo "Total Panes: $pane_count"
    
    # Show coordinate bounds for each pane
    echo ""
    echo "Pane Boundaries:"
    tmux list-panes -t "$SESSION:$WINDOW" -F "#{pane_index}:#{pane_left}:#{pane_top}:#{pane_width}:#{pane_height}" | \
    while IFS=':' read -r idx left top width height; do
        local right=$((left + width))
        local bottom=$((top + height))
        echo "  Pane $idx: x[$left-$right] y[$top-$bottom]"
    done
    
    echo "=== End Snapshot ==="
}

# Handle output modes
case "$OUTPUT_MODE" in
    console)
        generate_layout
        ;;
        
    pane:*)
        TARGET_PANE="${OUTPUT_MODE#pane:}"
        TEMP_FILE="/tmp/tmux-layout-$$.txt"
        generate_layout > "$TEMP_FILE"
        
        # Clear target pane and display layout
        tmux send-keys -t "$TARGET_PANE" C-c
        sleep 0.1
        tmux send-keys -t "$TARGET_PANE" "clear; cat $TEMP_FILE; rm -f $TEMP_FILE" Enter
        ;;
        
    both:*)
        TARGET_PANE="${OUTPUT_MODE#both:}"
        # Output to console
        generate_layout
        
        # Also send to pane
        TEMP_FILE="/tmp/tmux-layout-$$.txt"
        generate_layout > "$TEMP_FILE"
        tmux send-keys -t "$TARGET_PANE" C-c
        sleep 0.1
        tmux send-keys -t "$TARGET_PANE" "clear; cat $TEMP_FILE; rm -f $TEMP_FILE" Enter
        ;;
        
    *)
        echo "Unknown output mode: $OUTPUT_MODE"
        echo "Valid modes: console, pane:TARGET, both:TARGET"
        exit 1
        ;;
esac