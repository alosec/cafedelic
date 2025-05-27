#!/usr/bin/env bash

# tmex-pane-carousel.sh - Demonstrate tmex layouts by splitting a target pane
# Usage: ./tmex-pane-carousel.sh [target-pane] [delay-seconds]

set -euo pipefail

# Default values
if [[ $# -lt 1 ]]; then
    # If no target specified, use current window pane 1
    TARGET_PANE="$(tmux display-message -p '#{session_name}:#{window_index}').1"
else
    TARGET_PANE="${1}"
fi
DELAY="${2:-3}"         # Default to 3 seconds between layouts

# Get the window containing the target pane
TARGET_WINDOW=$(tmux display-message -t "${TARGET_PANE}" -p '#{session_name}:#{window_index}' 2>/dev/null || echo ".")

# Array of tmex layouts to demonstrate
LAYOUTS=(
    # Simple layouts
    "1"       # Single pane (no split)
    "11"      # Two columns
    "111"     # Three columns
    "1111"    # Four columns
    "12"      # One column, two in second
    "122"     # One, two, two
    "1234"    # Progressive columns
    
    # Complex layouts with sublayouts
    "1[23]4"  # Sublayout in middle column
    "12[34]5" # Sublayout with 3-4 split
    "[12]3"   # Sublayout at start
    "1[234]"  # Sublayout at end
    
    # Layouts with sizing
    "1{23}4"  # Middle column with 2:3 ratio
    "11{34}"  # Last two columns with 3:4 ratio
    "{12}34"  # First two columns with 1:2 ratio
    
    # Mixed sublayouts and sizing
    "1[2{34}]5"     # Nested sublayout with sizing
    "[1{23}4]5"     # Complex sublayout at start
    "12[3{45}6]"    # Complex sublayout in middle
    
    # Grid patterns (using {+} expansion)
    "4{+}"    # 2x2 grid
    "9{+}"    # 3x3 grid
    "6{+}"    # 2x3 or 3x2 grid
    
    # Transpose examples (would need -t flag)
    "213"     # Asymmetric layout
    "1213"    # Another asymmetric pattern
)

echo "=== TMEX Pane Carousel ==="
echo "Target pane: ${TARGET_PANE}"
echo "Target window: ${TARGET_WINDOW}"
echo "Delay between layouts: ${DELAY}s"
echo "Total layouts to demonstrate: ${#LAYOUTS[@]}"
echo ""
echo "This will split pane ${TARGET_PANE} into various layouts"
echo "Pane 0 will remain untouched as the control pane"
echo ""
echo "Press Ctrl-C to stop"
echo ""

# Function to reset to just panes 0 and 1
reset_panes() {
    ./scripts/tmux-clear.sh "${TARGET_WINDOW}" reset-keep-two >/dev/null 2>&1
    sleep 0.5
}

# Function to clear the target pane
clear_pane() {
    tmux send-keys -t "${TARGET_PANE}" C-c
    sleep 0.2
    tmux send-keys -t "${TARGET_PANE}" "clear" Enter
    sleep 0.2
}

# Initial reset
echo "Initial reset to panes 0 and 1..."
reset_panes

# Main carousel loop
for i in "${!LAYOUTS[@]}"; do
    layout="${LAYOUTS[$i]}"
    echo ""
    echo "[$((i + 1))/${#LAYOUTS[@]}] Demonstrating layout: ${layout}"
    
    # Make sure we're starting clean
    clear_pane
    
    # Select the target pane before sending tmex
    tmux select-pane -t "${TARGET_PANE}"
    
    # Send the tmex command (without session name, to split current pane)
    tmux send-keys -t "${TARGET_PANE}" "tmex ${layout}" Enter
    
    # Wait a moment for layout to render
    sleep 0.5
    
    # Capture the layout snapshot
    echo ""
    ./scripts/capture-tmux-layout.sh "${TARGET_WINDOW}" console
    
    # Let the layout display for viewing
    sleep "${DELAY}"
    
    # Reset back to just panes 0 and 1
    echo ""
    echo "  Resetting to panes 0 and 1..."
    reset_panes
done

echo ""
echo "Carousel complete!"
echo "Final cleanup..."
reset_panes
clear_pane