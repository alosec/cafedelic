#!/usr/bin/env bash

# tmux-clear.sh - Clear/reset a tmux pane or session
# Usage: ./tmux-clear.sh [target] [mode]
# target: pane identifier (e.g., %1, session:window.pane) or session name
# mode: "pane" (default), "session", "reset-layout", or "reset-keep-two"

set -euo pipefail

TARGET="${1:-%0}"  # Default to current pane
MODE="${2:-pane}"  # Default to pane mode

case "$MODE" in
    pane)
        echo "Clearing pane: ${TARGET}"
        
        # First, try to kill any tmux session that might be running in the pane
        # Get the command running in the pane
        PANE_CMD=$(tmux display-message -t "${TARGET}" -p '#{pane_current_command}' 2>/dev/null || echo "")
        
        if [[ "$PANE_CMD" == "tmux" ]]; then
            echo "Found tmux running in pane, sending exit commands..."
            # Send Ctrl-C to interrupt any running command
            tmux send-keys -t "${TARGET}" C-c
            sleep 0.1
            # Try to exit the tmux session
            tmux send-keys -t "${TARGET}" "exit" Enter
            sleep 0.1
        fi
        
        # Send Ctrl-C to interrupt any running command
        tmux send-keys -t "${TARGET}" C-c
        sleep 0.1
        
        # Clear the screen
        tmux send-keys -t "${TARGET}" "clear" Enter
        
        # Reset the pane to bash prompt
        tmux send-keys -t "${TARGET}" "exec bash" Enter
        ;;
        
    session)
        echo "Killing session: ${TARGET}"
        # Kill the entire session
        if tmux has-session -t "${TARGET}" 2>/dev/null; then
            tmux kill-session -t "${TARGET}"
            echo "Session ${TARGET} killed"
        else
            echo "Session ${TARGET} not found"
        fi
        ;;
        
    reset-layout)
        # Reset the current window layout to a single pane
        echo "Resetting window layout..."
        WINDOW="${TARGET}"
        
        # Get all panes in the window except the first one
        PANES=$(tmux list-panes -t "${WINDOW}" -F '#{pane_index}' | tail -n +2)
        
        # Kill all panes except the first
        for pane in $PANES; do
            tmux kill-pane -t "${WINDOW}.${pane}" 2>/dev/null || true
        done
        
        echo "Window reset to single pane"
        ;;
        
    reset-keep-two)
        # Reset the current window layout keeping only panes 0 and 1
        echo "Resetting window layout (keeping panes 0 and 1)..."
        WINDOW="${TARGET}"
        
        # Get all panes in the window
        PANES=$(tmux list-panes -t "${WINDOW}" -F '#{pane_index}')
        
        # Kill all panes except 0 and 1
        for pane in $PANES; do
            if [[ "$pane" != "0" && "$pane" != "1" ]]; then
                tmux kill-pane -t "${WINDOW}.${pane}" 2>/dev/null || true
            fi
        done
        
        # Count remaining panes
        REMAINING=$(tmux list-panes -t "${WINDOW}" | wc -l)
        if [[ "$REMAINING" == "1" ]]; then
            echo "Window has only 1 pane (no pane 1 to preserve)"
        else
            echo "Window reset to panes 0 and 1"
        fi
        ;;
        
    *)
        echo "Unknown mode: $MODE"
        echo "Valid modes: pane, session, reset-layout, reset-keep-two"
        exit 1
        ;;
esac