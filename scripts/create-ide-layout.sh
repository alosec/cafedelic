#!/bin/bash

# Cafedelic IDE Layout Creation Script
# Creates the IDE layout using tmex 131 --transpose

SESSION="${1:-cafedelic-session}"
WINDOW_NAME="${2:-cafedelic}"
PROJECT_DIR="${3:-$(pwd)}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "🚀 Creating Cafedelic IDE layout..."
echo "   Session: $SESSION"
echo "   Window: $WINDOW_NAME"
echo "   Project: $PROJECT_DIR"
echo ""

# Kill existing session if it exists
if tmux has-session -t "$SESSION" 2>/dev/null; then
    echo "🧹 Cleaning up existing session '$SESSION'..."
    tmux kill-session -t "$SESSION"
fi

# Create the layout using tmex 131 --transpose
# This creates: 1 row on top, 3 columns in middle, 1 row on bottom
echo "🔨 Creating layout with tmex..."
"$SCRIPT_DIR/tmex" "$SESSION" --layout="131" --transpose \
    "clear && echo '🎯 Activity Monitor'; echo 'Ready for future features...'" \
    "clear && echo '🌳 Loading file tree...'" \
    "if ! pgrep -x 'emacs' > /dev/null; then emacs --daemon; fi && emacsclient -nw -c" \
    "clear && echo '💻 Terminal Ready'" \
    "clear && echo '📊 Starting DC log monitor...'; cd '$PROJECT_DIR' && node ./scripts/monitor-dc-logs.js"

# Give tmux a moment to settle
sleep 0.5

# Rename the window
tmux rename-window -t "$SESSION:0" "$WINDOW_NAME"

# Set pane names for identification (not titles)
# Using set-option to set user options that can be referenced
echo "📝 Setting pane names..."
tmux set-option -t "${SESSION}:${WINDOW_NAME}.0" @pane_name "cafedelic-activity"
tmux set-option -t "${SESSION}:${WINDOW_NAME}.1" @pane_name "cafedelic-files"
tmux set-option -t "${SESSION}:${WINDOW_NAME}.2" @pane_name "cafedelic-editor"
tmux set-option -t "${SESSION}:${WINDOW_NAME}.3" @pane_name "cafedelic-terminal"
tmux set-option -t "${SESSION}:${WINDOW_NAME}.4" @pane_name "cafedelic-logs"

# Set pane borders to match global config
echo "🎨 Configuring pane appearance..."
tmux set-option -w -t "$SESSION:$WINDOW_NAME" pane-active-border-style 'fg=colour82,bg=colour235,bold' 2>/dev/null || true
tmux set-option -w -t "$SESSION:$WINDOW_NAME" pane-border-style 'fg=colour238,bg=colour235' 2>/dev/null || true

# Start the file tree viewer in pane 1
echo "🌳 Starting file tree..."
tmux send-keys -t "${SESSION}:${WINDOW_NAME}.1" C-c
tmux send-keys -t "${SESSION}:${WINDOW_NAME}.1" "watch -n 5 '${SCRIPT_DIR}/generate-file-tree.sh ${PROJECT_DIR}'" Enter

# Select the editor pane as the active pane
tmux select-pane -t "${SESSION}:${WINDOW_NAME}.2"

echo ""
echo "✅ IDE layout created successfully!"
echo ""
echo "📋 Panes created with names:"
echo "  - Pane 0 (@pane_name=cafedelic-activity): Activity Monitor (top)"
echo "  - Pane 1 (@pane_name=cafedelic-files): File Tree (left)" 
echo "  - Pane 2 (@pane_name=cafedelic-editor): Editor (center)"
echo "  - Pane 3 (@pane_name=cafedelic-terminal): Terminal (right)"
echo "  - Pane 4 (@pane_name=cafedelic-logs): DC Logs (bottom)"
echo ""
echo "🔗 To attach: tmux attach -t $SESSION"
echo "🗑️  To cleanup: tmux kill-session -t $SESSION"
echo ""
echo "💡 Tip: Reference panes by name with: tmux display -p -t '@pane_name=cafedelic-editor' '#{pane_id}'"
