#!/bin/bash

# Tmex Pane Tester - Apply tmex layouts to adjacent pane for testing
# Usage: ./test-pane.sh
# Requirements: Must be run in a tmux session with at least 2 panes

# Check if we're in tmux
if [ -z "$TMUX" ]; then
    echo "Error: This script must be run inside a tmux session"
    echo "Try: tmux new-session \\; split-window -h \\; send-keys './scripts/tmex-tools/test-pane.sh' Enter"
    exit 1
fi

# Get script directory for tmex path
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TMEX_PATH="$SCRIPT_DIR/../tmex"

# Get current pane info
CURRENT_PANE=$(tmux display-message -p '#{pane_id}')
WINDOW_PANES=$(tmux list-panes -F '#{pane_id}')
PANE_COUNT=$(echo "$WINDOW_PANES" | wc -l)

if [ "$PANE_COUNT" -lt 2 ]; then
    echo "Error: Need at least 2 panes in current window"
    echo "Create a split with: Ctrl-b %"
    exit 1
fi

# Function to get the other pane ID
get_other_pane() {
    echo "$WINDOW_PANES" | grep -v "^$CURRENT_PANE$" | head -1
}

# Function to apply layout to other pane
apply_layout() {
    local pattern="$1"
    local transpose=""
    local other_pane=$(get_other_pane)
    
    # Check for transpose flag
    if [[ "$pattern" == *" -t"* ]] || [[ "$pattern" == *" --transpose"* ]]; then
        transpose="--transpose"
        pattern="${pattern%% -*}"  # Remove flag from pattern
    fi
    
    # Count how many panes we need
    local pane_count=0
    for (( i=0; i<${#pattern}; i++ )); do
        char="${pattern:$i:1}"
        if [[ "$char" =~ [0-9] ]]; then
            pane_count=$((pane_count + char))
        fi
    done
    
    # Clear the other pane first
    clear_pane silent
    
    # Select the other pane
    tmux select-pane -t "$other_pane"
    
    # Generate commands for each pane (simple numbered displays)
    local commands=()
    for (( i=1; i<=pane_count; i++ )); do
        commands+=("echo -e '\\n\\n\\033[1;36m      Pane $i\\033[0m\\n'; echo '      Pattern: $pattern'")
    done
    
    # Apply the layout using tmex
    if [ ${#commands[@]} -gt 0 ]; then
        echo "Applying layout: $pattern $transpose"
        "$TMEX_PATH" temp-test --layout="$pattern" $transpose "${commands[@]}" 2>/dev/null || {
            echo "Error: Invalid pattern or tmex failed"
        }
    else
        echo "Error: No valid panes in pattern"
    fi
    
    # Return to original pane
    tmux select-pane -t "$CURRENT_PANE"
}

# Function to clear other pane (reset to single pane)
clear_pane() {
    local silent="$1"
    local other_pane=$(get_other_pane)
    
    # Kill all panes in the window except current and target
    while read -r pane_id; do
        if [[ "$pane_id" != "$CURRENT_PANE" && "$pane_id" != "$other_pane" ]]; then
            tmux kill-pane -t "$pane_id" 2>/dev/null
        fi
    done <<< "$WINDOW_PANES"
    
    [ "$silent" != "silent" ] && echo "Cleared layout"
}

# Print header
clear
echo "🧪 Tmex Pane Tester"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Type a tmex pattern to test it in the other pane"
echo ""
echo "Commands:"
echo "  [pattern]     - Apply tmex layout (e.g., 113, 1[123]1)"
echo "  [pattern] -t  - Apply transposed layout"
echo "  clear         - Reset to single pane"
echo "  help          - Show example patterns"
echo "  quit/exit     - Exit tester"
echo ""
echo "Examples: 113, 122, 131, 1[123]1, 14{1,1,1,2}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Main loop
while true; do
    read -r -p "tmex> " cmd
    
    # Handle empty input
    [ -z "$cmd" ] && continue
    
    case "$cmd" in
        quit|exit|q)
            echo "Goodbye!"
            break
            ;;
        clear|c)
            clear_pane
            ;;
        help|h|?)
            echo ""
            echo "📚 Example Patterns:"
            echo "  113         - 3 columns: 1 + 1 + 3 panes"
            echo "  122         - 3 columns: 1 + 2 + 2 panes"
            echo "  131         - 3 columns: 1 + 3 + 1 panes"
            echo "  1111        - 4 equal columns"
            echo "  1[123]1     - 3 columns: 1 + [1,2,3 rows] + 1"
            echo "  2{1,3}2     - 3 columns: 2 + 2 (but middle is 1:3 ratio)"
            echo "  [12][34]    - 2 columns, each with 2 different-sized rows"
            echo ""
            echo "Add -t or --transpose to any pattern for horizontal layout"
            echo ""
            ;;
        *)
            apply_layout "$cmd"
            ;;
    esac
done
