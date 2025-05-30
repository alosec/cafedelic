#!/bin/bash
# send-special-key.sh - Send special key sequences to a named pane
# Usage: ./send-special-key.sh NAME KEY

set -e

NAME="$1"
KEY="$2"

if [ -z "$NAME" ] || [ -z "$KEY" ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 NAME KEY"
    echo "Valid keys: enter, escape, tab, ctrl-c, ctrl-d, ctrl-z, up, down, left, right, home, end, page-up, page-down"
    exit 1
fi

# Find the pane coordinates
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COORDS=$("$SCRIPT_DIR/../utils/find-pane.sh" "$NAME")

if [ $? -ne 0 ]; then
    echo "$COORDS"  # Error message from find-pane
    exit 1
fi

# Map friendly names to tmux key names
case "${KEY,,}" in  # Convert to lowercase
    enter) TMUX_KEY="Enter" ;;
    escape|esc) TMUX_KEY="Escape" ;;
    tab) TMUX_KEY="Tab" ;;
    ctrl-c) TMUX_KEY="C-c" ;;
    ctrl-d) TMUX_KEY="C-d" ;;
    ctrl-z) TMUX_KEY="C-z" ;;
    ctrl-a) TMUX_KEY="C-a" ;;
    ctrl-e) TMUX_KEY="C-e" ;;
    ctrl-k) TMUX_KEY="C-k" ;;
    ctrl-l) TMUX_KEY="C-l" ;;
    ctrl-u) TMUX_KEY="C-u" ;;
    ctrl-w) TMUX_KEY="C-w" ;;
    up) TMUX_KEY="Up" ;;
    down) TMUX_KEY="Down" ;;
    left) TMUX_KEY="Left" ;;
    right) TMUX_KEY="Right" ;;
    home) TMUX_KEY="Home" ;;
    end) TMUX_KEY="End" ;;
    page-up|pageup|pgup) TMUX_KEY="PageUp" ;;
    page-down|pagedown|pgdn) TMUX_KEY="PageDown" ;;
    space) TMUX_KEY="Space" ;;
    backspace|bksp) TMUX_KEY="BSpace" ;;
    delete|del) TMUX_KEY="DC" ;;
    f1) TMUX_KEY="F1" ;;
    f2) TMUX_KEY="F2" ;;
    f3) TMUX_KEY="F3" ;;
    f4) TMUX_KEY="F4" ;;
    f5) TMUX_KEY="F5" ;;
    f6) TMUX_KEY="F6" ;;
    f7) TMUX_KEY="F7" ;;
    f8) TMUX_KEY="F8" ;;
    f9) TMUX_KEY="F9" ;;
    f10) TMUX_KEY="F10" ;;
    f11) TMUX_KEY="F11" ;;
    f12) TMUX_KEY="F12" ;;
    *)
        echo "Error: Unknown key '${KEY}'"
        echo "Valid keys: enter, escape, tab, ctrl-c, ctrl-d, ctrl-z, up, down, left, right, home, end, page-up, page-down"
        exit 1
        ;;
esac

# Send the key to the pane
tmux send-keys -t "$COORDS" "$TMUX_KEY"

echo "Sent ${KEY} to pane '${NAME}' (${COORDS})"
