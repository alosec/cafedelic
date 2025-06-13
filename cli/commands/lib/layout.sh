#!/bin/bash
# Cafedelic CLI - Layout management functions

# Create 2-pane vertical layout (70/30)
create_2pane_layout() {
    local session="${1:-$(get_current_tmux_session)}"
    local window="${2:-0}"
    
    log "Creating 2-pane layout in session: $session"
    
    # Check current pane count
    local current_panes
    current_panes=$(tmux list-panes -t "$session:$window" 2>/dev/null | wc -l)
    
    if [ "$current_panes" -ge 2 ]; then
        log "Layout already has $current_panes panes, skipping split"
        success "2-pane layout ready"
        return 0
    fi
    
    # Split window horizontally (70% top, 30% bottom)
    if ! tmux split-window -v -p 30 -t "$session:$window"; then
        error "Failed to split window"
        return 1
    fi
    
    success "2-pane layout created"
    return 0
}

# Assign pane properties using existing script
assign_pane_properties() {
    local session="$1"
    local window="$2"
    local pane="$3"
    local name="$4"
    local source="$5"
    local role="$6"
    
    local script_path="$CAFEDELIC_DIR/scripts/pane-properties/assign-properties.sh"
    
    if [ ! -x "$script_path" ]; then
        error "assign-properties.sh script not found or not executable"
        return 1
    fi
    
    log "Assigning properties to pane $session:$window.$pane"
    
    # Call the existing script with correct parameter format
    if "$script_path" "$session" "$window" "$pane" --name "$name" --source "$source" --role "$role"; then
        success "Properties assigned: name=$name, source=$source, role=$role"
        return 0
    else
        error "Failed to assign pane properties"
        return 1
    fi
}
