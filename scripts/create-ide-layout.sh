#!/bin/bash

# Cafedelic IDE Layout Creation Script
# Creates the IDE layout in a tmux session

create_cafedelic_layout() {
    local SESSION="${1:-0}"
    local WINDOW_NAME="${2:-cafedelic}"
    
    echo "Creating Cafedelic IDE layout in $SESSION:$WINDOW_NAME..."
    
    # Create new window or select existing
    if ! tmux list-windows -t "$SESSION" -F "#{window_name}" | grep -q "^${WINDOW_NAME}$"; then
        tmux new-window -t "$SESSION" -n "$WINDOW_NAME"
    else
        tmux select-window -t "$SESSION:$WINDOW_NAME"
        # Kill all panes except first
        tmux kill-pane -a -t "$SESSION:$WINDOW_NAME.0"
    fi
    
    # Create the layout structure
    # First split: Create top row (activity monitor - 10%)
    tmux split-window -t "$SESSION:$WINDOW_NAME.0" -v -l 90%
    
    # After first split: pane 0 (top 10%), pane 1 (bottom 90%)
    # Second split: Split pane 1 to create the bottom logs section
    tmux select-pane -t "$SESSION:$WINDOW_NAME.1"
    tmux split-window -v -l 11%
    
    # After second split: pane 0 (top 10%), pane 1 (middle 80%), pane 2 (bottom 10%)
    # Split the middle section (pane 1) horizontally for editor and terminal
    tmux select-pane -t "$SESSION:$WINDOW_NAME.1"
    tmux split-window -h -l 50%
    
    # Final layout: pane 0 (activity), pane 1 (editor), pane 2 (terminal), pane 3 (logs)
    
    # Set pane titles for named pane access
    tmux select-pane -t "$SESSION:$WINDOW_NAME.0" -T "cafedelic-activity"
    tmux select-pane -t "$SESSION:$WINDOW_NAME.1" -T "cafedelic-editor"
    tmux select-pane -t "$SESSION:$WINDOW_NAME.2" -T "cafedelic-terminal"
    tmux select-pane -t "$SESSION:$WINDOW_NAME.3" -T "cafedelic-logs"
    
    # Configure pane borders
    tmux set-option -w -t "$SESSION:$WINDOW_NAME" pane-border-status top
    tmux set-option -w -t "$SESSION:$WINDOW_NAME" pane-border-format '#[fg=colour244,bg=colour235,bold] #{pane_title} #[default]'
    tmux set-option -w -t "$SESSION:$WINDOW_NAME" pane-active-border-style 'fg=colour82,bg=colour235,bold'
    tmux set-option -w -t "$SESSION:$WINDOW_NAME" pane-border-style 'fg=colour238,bg=colour235'
    
    # Optional: Start processes
    if [ "${3:-yes}" = "yes" ]; then
        echo "Starting processes..."
        
        # Activity Monitor - Just clear for now
        tmux send-keys -t "$SESSION:$WINDOW_NAME.0" 'clear && echo "Cafedelic Activity Monitor"' Enter
        
        # Emacs Editor
        tmux send-keys -t "$SESSION:$WINDOW_NAME.1" 'if ! pgrep -x "emacs" > /dev/null; then emacs --daemon; fi && emacsclient -nw -c' Enter
        
        # Terminal - just clear
        tmux send-keys -t "$SESSION:$WINDOW_NAME.2" 'clear && echo "Cafedelic Terminal"' Enter
        
        # Context/Logs - clear for now
        tmux send-keys -t "$SESSION:$WINDOW_NAME.3" 'clear && echo "Cafedelic Context/Logs"' Enter
    fi
    
    # Select the editor pane
    tmux select-pane -t "$SESSION:$WINDOW_NAME.1"
    
    # Save the layout string for reference
    LAYOUT=$(tmux list-windows -t "$SESSION:$WINDOW_NAME" -F "#{window_layout}")
    echo "Layout created successfully!"
    echo "Layout string: $LAYOUT"
    echo ""
    echo "Named panes created:"
    echo "  - cafedelic-activity"
    echo "  - cafedelic-editor"
    echo "  - cafedelic-terminal"
    echo "  - cafedelic-logs"
}

# Execute
create_cafedelic_layout "$@"
