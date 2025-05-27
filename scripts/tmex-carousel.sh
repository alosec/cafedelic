#!/usr/bin/env bash

# tmex-carousel.sh - Demonstrate various tmex layouts in sequence
# Usage: ./tmex-carousel.sh [target-pane] [delay-seconds]

set -euo pipefail

# Default values
TARGET_PANE="${1:-%1}"  # Default to pane %1 if not specified
DELAY="${2:-3}"         # Default to 3 seconds between layouts

# Array of tmex layouts to demonstrate
LAYOUTS=(
    # Simple layouts
    "1"       # Single pane
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

echo "=== TMEX Layout Carousel ==="
echo "Target pane: ${TARGET_PANE}"
echo "Delay between layouts: ${DELAY}s"
echo "Total layouts to demonstrate: ${#LAYOUTS[@]}"
echo ""
echo "Press Ctrl-C to stop"
echo ""

# Function to clear the target pane
clear_pane() {
    tmux send-keys -t "${TARGET_PANE}" C-c Enter
    sleep 0.5
    tmux send-keys -t "${TARGET_PANE}" "clear" Enter
    sleep 0.5
}

# Main carousel loop
for i in "${!LAYOUTS[@]}"; do
    layout="${LAYOUTS[$i]}"
    echo "[$((i + 1))/${#LAYOUTS[@]}] Demonstrating layout: ${layout}"
    
    # Clear the pane first
    clear_pane
    
    # Send the tmex command
    tmux send-keys -t "${TARGET_PANE}" "tmex demo-${i} ${layout}" Enter
    
    # Wait before next layout
    sleep "${DELAY}"
    
    # Kill the tmux session created by tmex
    tmux send-keys -t "${TARGET_PANE}" C-c Enter
    sleep 0.5
    tmux send-keys -t "${TARGET_PANE}" "tmux kill-session -t demo-${i} 2>/dev/null || true" Enter
    sleep 0.5
done

echo ""
echo "Carousel complete!"
clear_pane