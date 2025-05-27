#!/usr/bin/env bash

# tmex-visual-carousel.sh - Carousel with live visualization in control pane
# Usage: ./tmex-visual-carousel.sh [delay-seconds]

set -euo pipefail

DELAY="${1:-3}"  # Default to 3 seconds between layouts

# Get current window info
CURRENT_WINDOW=$(tmux display-message -p '#{session_name}:#{window_index}')
CONTROL_PANE="${CURRENT_WINDOW}.0"
DEMO_PANE="${CURRENT_WINDOW}.1"

# Array of tmex layouts to demonstrate
LAYOUTS=(
    # Simple progression
    "1"       # Single pane
    "11"      # Two columns
    "111"     # Three columns
    "1111"    # Four columns
    
    # Varied column heights
    "12"      # One, two
    "122"     # One, two, two
    "1234"    # Progressive
    
    # Sublayouts
    "1[23]4"  # Sublayout in middle
    "[12]3"   # Sublayout at start
    "1[234]"  # Sublayout at end
    
    # Sizing
    "1{23}4"  # 2:3 ratio
    "11{34}"  # 3:4 ratio
    
    # Complex
    "1[2{34}]5"  # Nested with sizing
    "4{+}"       # 2x2 grid
    "9{+}"       # 3x3 grid
)

echo "=== TMEX Visual Carousel ==="
echo "Control Pane: ${CONTROL_PANE} (this pane)"
echo "Demo Pane: ${DEMO_PANE}"
echo "Delay: ${DELAY}s"
echo ""
echo "Starting in 2 seconds..."
sleep 2

# Function to reset to just panes 0 and 1
reset_panes() {
    ./scripts/tmux-clear.sh "${CURRENT_WINDOW}" reset-keep-two >/dev/null 2>&1
    sleep 0.3
}

# Function to display info in control pane
show_in_control() {
    local layout="$1"
    local index="$2"
    local total="$3"
    
    # Clear control pane
    clear
    
    echo "╔═══════════════════════════════════════════════════╗"
    echo "║           TMEX Layout Visualizer                  ║"
    echo "╚═══════════════════════════════════════════════════╝"
    echo ""
    echo "Layout [$index/$total]: $layout"
    echo ""
    
    # Capture and display layout
    ./scripts/capture-tmux-layout.sh "${CURRENT_WINDOW}" console
}

# Initial reset
reset_panes

# Main carousel loop
for i in "${!LAYOUTS[@]}"; do
    layout="${LAYOUTS[$i]}"
    
    # Clear demo pane
    tmux send-keys -t "${DEMO_PANE}" C-c
    sleep 0.1
    tmux send-keys -t "${DEMO_PANE}" "clear" Enter
    sleep 0.1
    
    # Send tmex command to demo pane
    tmux send-keys -t "${DEMO_PANE}" "tmex ${layout}" Enter
    
    # Wait for layout to render
    sleep 0.5
    
    # Display visualization in control pane
    show_in_control "$layout" "$((i + 1))" "${#LAYOUTS[@]}"
    
    # Hold for viewing
    sleep "${DELAY}"
    
    # Reset for next iteration
    reset_panes
done

# Final message
clear
echo "=== Carousel Complete ==="
echo ""
echo "Demonstrated ${#LAYOUTS[@]} layouts"
echo ""
echo "You can run specific layouts with:"
echo "  ./scripts/tmex-in-pane.sh LAYOUT ${DEMO_PANE}"
echo ""
echo "Or capture any layout with:"
echo "  ./scripts/capture-tmux-layout.sh"