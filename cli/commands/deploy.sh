#!/bin/bash
# Cafedelic CLI - deploy command
# Create 2-pane IDE layout (70% emacs, 30% system events)

set -euo pipefail

# Load libraries
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"
source "$SCRIPT_DIR/lib/layout.sh"

# Deploy 2-pane layout
deploy_layout() {
    local session="${1:-$(get_current_tmux_session)}"
    local window="${2:-0}"
    
    if [ -z "$session" ]; then
        error "No tmux session found. Start tmux first."
        return 1
    fi
    
    log "Deploying 2-pane layout to session: $session"
    
    # Create the layout
    if ! create_2pane_layout "$session" "$window"; then
        return 1
    fi
    
    # Get current panes count
    local pane_count
    pane_count=$(tmux list-panes -t "$session:$window" | wc -l)
    
    log "Pane count validation: found $pane_count panes"
    
    if [ "$pane_count" -lt 2 ]; then
        error "Layout creation failed - insufficient panes (found: $pane_count)"
        return 1
    fi
    
    # Assign properties to panes
    # Top pane (0) - Emacs editor
    assign_pane_properties "$session" "$window" "0" "editor" "claude-desktop" "editor"
    
    # Bottom pane (1) - System events
    assign_pane_properties "$session" "$window" "1" "system-events" "system" "monitor"
    
    # Start emacs in top pane
    log "Starting Emacs in editor pane..."
    tmux send-keys -t "$session:$window.0" "emacs" Enter
    
    # Start system events viewer in bottom pane
    log "Starting system events viewer..."
    tmux send-keys -t "$session:$window.1" "echo 'System Events Monitor - Ready'" Enter
    tmux send-keys -t "$session:$window.1" "echo 'Waiting for activity...'" Enter
    
    success "2-pane IDE layout deployed successfully!"
    success "Top pane: Emacs editor (70%)"
    success "Bottom pane: System events (30%)"
    
    return 0
}

main() {
    # Validate tmux environment first
    if ! validate_tmux; then
        return 1
    fi
    
    deploy_layout "$@"
}

main "$@"
