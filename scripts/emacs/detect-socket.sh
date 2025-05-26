#!/bin/bash
# detect-socket.sh - Find active Emacs socket across platforms

# Platform detection
PLATFORM="$(uname -s)"
SOCKET_NAME="${1:-}"

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Function to check if socket is valid
check_socket() {
    local socket_path="$1"
    if [ -S "$socket_path" ]; then
        # Test if we can connect
        if emacsclient --socket-name="$socket_path" --eval "(emacs-pid)" >/dev/null 2>&1; then
            echo "$socket_path"
            return 0
        fi
    fi
    return 1
}

# Function to find sockets in a directory
scan_directory() {
    local dir="$1"
    if [ -d "$dir" ]; then
        for socket in "$dir"/*; do
            if [ -S "$socket" ]; then
                if [ -z "$SOCKET_NAME" ] || [[ "$(basename "$socket")" == *"$SOCKET_NAME"* ]]; then
                    if check_socket "$socket"; then
                        return 0
                    fi
                fi
            fi
        done
    fi
    return 1
}

# Check environment variable first
if [ -n "$EMACS_SOCKET_NAME" ]; then
    if check_socket "$EMACS_SOCKET_NAME"; then
        exit 0
    fi
fi

# Platform-specific search paths
case "$PLATFORM" in
    Linux)
        # Check XDG_RUNTIME_DIR first
        if [ -n "$XDG_RUNTIME_DIR" ]; then
            scan_directory "$XDG_RUNTIME_DIR/emacs" && exit 0
        fi
        
        # Traditional Linux paths
        scan_directory "/run/user/$(id -u)/emacs" && exit 0
        scan_directory "/tmp/emacs$(id -u)" && exit 0
        scan_directory "$HOME/.emacs.d/server" && exit 0
        ;;
        
    Darwin)
        # macOS paths
        scan_directory "$HOME/Library/Emacs" && exit 0
        scan_directory "/tmp/emacs$(id -u)" && exit 0
        scan_directory "$HOME/.emacs.d/server" && exit 0
        
        # Check TMPDIR
        if [ -n "$TMPDIR" ]; then
            scan_directory "$TMPDIR/emacs" && exit 0
        fi
        
        # Try process scanning as fallback
        if command -v lsof >/dev/null 2>&1; then
            SOCKET=$(lsof -U -a -c emacs 2>/dev/null | grep LISTEN | awk '{print $NF}' | head -1)
            if [ -n "$SOCKET" ] && check_socket "$SOCKET"; then
                echo "$SOCKET"
                exit 0
            fi
        fi
        ;;
        
    MINGW*|CYGWIN*|MSYS*)
        # Windows - try emacsclient directly
        if [ -n "$SOCKET_NAME" ]; then
            if emacsclient --socket-name="$SOCKET_NAME" --eval "(emacs-pid)" >/dev/null 2>&1; then
                echo "\\\\.\pipe\\$SOCKET_NAME"
                exit 0
            fi
        else
            # Try default server
            if emacsclient --eval "(emacs-pid)" >/dev/null 2>&1; then
                echo "\\\\.\pipe\\server"
                exit 0
            fi
        fi
        ;;
esac

# No socket found
exit 1