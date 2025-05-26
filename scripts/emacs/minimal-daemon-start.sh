#!/bin/bash
# minimal-daemon-start.sh - Start Emacs daemon with minimal configuration

SOCKET_NAME="${1:-cafedelic}"
EMACS="${EMACS:-emacs}"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${YELLOW}Starting Emacs daemon: $SOCKET_NAME${NC}"

# Check if daemon already exists
if emacsclient --socket-name="$SOCKET_NAME" --eval "(emacs-pid)" >/dev/null 2>&1; then
    PID=$(emacsclient --socket-name="$SOCKET_NAME" --eval "(emacs-pid)" 2>/dev/null | tr -d '"')
    echo -e "${GREEN}Daemon already running with PID: $PID${NC}"
    exit 0
fi

# Ensure socket directory exists
case "$(uname -s)" in
    Linux)
        if [ -n "$XDG_RUNTIME_DIR" ]; then
            SOCKET_DIR="$XDG_RUNTIME_DIR/emacs"
        else
            SOCKET_DIR="/tmp/emacs$(id -u)"
        fi
        ;;
    Darwin)
        SOCKET_DIR="${TMPDIR:-/tmp}/emacs"
        ;;
    *)
        SOCKET_DIR="/tmp/emacs$(id -u)"
        ;;
esac

mkdir -p "$SOCKET_DIR"

# Start daemon with minimal config for fast startup
echo -e "${YELLOW}Starting daemon with minimal configuration...${NC}"
"$EMACS" --daemon="$SOCKET_NAME" \
         --quick \
         --no-site-file \
         --eval "(setq server-name \"$SOCKET_NAME\")" \
         2>/dev/null &

DAEMON_PID=$!

# Wait for daemon to be ready (max 30 seconds)
TIMEOUT=30
ELAPSED=0

while [ $ELAPSED -lt $TIMEOUT ]; do
    if emacsclient --socket-name="$SOCKET_NAME" --eval "(emacs-pid)" >/dev/null 2>&1; then
        PID=$(emacsclient --socket-name="$SOCKET_NAME" --eval "(emacs-pid)" 2>/dev/null | tr -d '"')
        echo -e "${GREEN}Daemon started successfully with PID: $PID${NC}"
        
        # Load Cafedelic configuration if it exists
        SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
        if [ -f "$SCRIPT_DIR/cafedelic-editor.el" ]; then
            echo -e "${YELLOW}Loading Cafedelic configuration...${NC}"
            emacsclient --socket-name="$SOCKET_NAME" \
                       --eval "(load \"$SCRIPT_DIR/cafedelic-editor.el\")" \
                       >/dev/null 2>&1
        fi
        
        exit 0
    fi
    
    sleep 0.5
    ELAPSED=$((ELAPSED + 1))
done

echo -e "${RED}Failed to start daemon within timeout${NC}"
exit 1