#!/bin/bash
# Cafedelic Launcher - Adapted from teemax-launcher.sh
# Launches stterm with a cafedelic tmux session

# Determine which st binary to use
ST_BINARY="st"
if [ -f "$HOME/bin/st" ]; then
    ST_BINARY="$HOME/bin/st"
fi

# Default settings
FONT_SIZE=12
SESSION_NAME="cafedelic-main"
SHELL="${SHELL:-bash}"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --session=*)
            SESSION_NAME="${1#*=}"
            shift
            ;;
        --size=*)
            FONT_SIZE="${1#*=}"
            shift
            ;;
        --shell=*)
            SHELL="${1#*=}"
            shift
            ;;
        bash|zsh|fish|sh)
            SHELL="$1"
            shift
            ;;
        *)
            # Any other argument becomes part of session name
            SESSION_NAME="cafedelic-$1"
            shift
            ;;
    esac
done

# Function to create or attach to cafedelic session
create_cafedelic_session() {
    local session="$1"
    local shell="$2"
    
    # Ensure we're not in a tmux session already
    unset TMUX
    
    # Check if session exists
    if tmux has-session -t "$session" 2>/dev/null; then
        echo "Attaching to existing session: $session"
        tmux attach-session -t "$session"
    else
        echo "Creating new session: $session"
        tmux new-session -s "$session" -n main "$shell"
    fi
}

# Export the function so it's available in the subshell
export -f create_cafedelic_session

# Launch st with bash, which will then run our tmux session
exec "$ST_BINARY" -f "DejaVu Sans Mono:size=$FONT_SIZE" -e bash -c "
    echo 'Cafedelic - Fractal recursive analysis IDE'
    echo 'Session: $SESSION_NAME'
    echo ''
    create_cafedelic_session '$SESSION_NAME' '$SHELL'
    exec bash
"
