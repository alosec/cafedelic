#!/bin/bash
# Cafedelic CLI - Common functions library

# Color output functions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log() {
    echo -e "${BLUE}[INFO]${NC} $1" >&2
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" >&2
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" >&2
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check if tmux session exists
tmux_session_exists() {
    tmux has-session -t "$1" 2>/dev/null
}

# Get current tmux session
get_current_tmux_session() {
    tmux display-message -p '#S' 2>/dev/null || echo ""
}

# Validate tmux environment
validate_tmux() {
    if ! command_exists tmux; then
        error "tmux not found. Install with: sudo apt install tmux"
        return 1
    fi
    
    if [ -z "${TMUX:-}" ]; then
        error "Not inside a tmux session. Start tmux first or run 'tmux new-session'"
        return 1
    fi
    
    return 0
}
