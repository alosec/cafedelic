#!/bin/bash
# open-dired-v2.sh - Open a directory in dired mode in the Claude emacs frame

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

# Use socket name from environment if available
SOCKET_ARG=""
if [ -n "$CAFEDELIC_SOCKET_NAME" ]; then
    SOCKET_ARG="--socket-name=$CAFEDELIC_SOCKET_NAME"
    log_debug "Using socket: $CAFEDELIC_SOCKET_NAME"
fi

# Open directory in dired mode
ELISP_CODE="(progn
  (dired \"$DIRECTORY_PATH\")
  (message \"Opened directory in dired: %s\" \"$DIRECTORY_PATH\")
  \"Directory opened in dired\")"

# Execute emacsclient command
emacsclient $SOCKET_ARG --eval "$ELISP_CODE" 2>&1 || {
    log_debug "Error: Failed to open directory in emacs"
    exit 1
}

log_debug "Successfully opened directory in dired"
echo "Opened: $DIRECTORY_PATH"
