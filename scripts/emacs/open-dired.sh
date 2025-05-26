#!/bin/bash
# open-dired.sh - Open a directory in dired mode in the Claude emacs frame

# Exit on error
set -e

# Function to log debug messages
log_debug() {
    echo "[open-dired] $1" >&2
}

# Check if directory path is provided
if [ -z "$1" ]; then
    log_debug "Error: No directory path provided"
    echo "Usage: $0 <directory-path>"
    exit 1
fi

DIRECTORY_PATH="$1"

# Convert to absolute path if relative
if [[ ! "$DIRECTORY_PATH" = /* ]]; then
    DIRECTORY_PATH="$(pwd)/$DIRECTORY_PATH"
fi

# Check if directory exists
if [ ! -d "$DIRECTORY_PATH" ]; then
    log_debug "Error: Directory does not exist: $DIRECTORY_PATH"
    exit 1
fi

log_debug "Opening directory: $DIRECTORY_PATH"

# Find the Claude pane (named 'sophie' or 'cafedelic-editor')
PANE_NAME="cafedelic-editor"
PANE_ID=$(tmux list-panes -a -F '#{pane_id} #{@pane_name}' | grep "$PANE_NAME" | awk '{print $1}' || true)

if [ -z "$PANE_ID" ]; then
    # Try alternate name
    PANE_NAME="sophie"
    PANE_ID=$(tmux list-panes -a -F '#{pane_id} #{@pane_name}' | grep "$PANE_NAME" | awk '{print $1}' || true)
fi

if [ -z "$PANE_ID" ]; then
    log_debug "Warning: Could not find Claude editor pane (cafedelic-editor or sophie)"
    # Fall back to using emacsclient without specific pane
    PANE_ID=""
fi

# Open directory in dired mode
ELISP_CODE="(progn
  (dired \"$DIRECTORY_PATH\")
  (message \"Opened directory in dired: %s\" \"$DIRECTORY_PATH\")
  \"Directory opened in dired\")"

# Always use emacsclient directly to evaluate the code
log_debug "Using emacsclient to open dired"
emacsclient --eval "$ELISP_CODE" 2>&1 || {
    log_debug "Error: Failed to open directory in emacs"
    exit 1
}

log_debug "Successfully opened directory in dired"
echo "Opened: $DIRECTORY_PATH"