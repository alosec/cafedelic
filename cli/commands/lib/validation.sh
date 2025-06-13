#!/bin/bash
# Cafedelic CLI - Validation functions

# Validate MCP server availability
validate_mcp_server() {
    local mcp_pid
    log "Checking MCP server..."
    
    # Check if MCP server process is running
    if pgrep -f "mcp-server" >/dev/null; then
        success "MCP server process found"
        return 0
    fi
    
    # Try to connect to HTTP server (port 3001)
    if curl -s -f "http://localhost:3001/health" >/dev/null 2>&1; then
        success "MCP HTTP server responding on port 3001"
        return 0
    fi
    
    warn "MCP server not detected. Start with: npm run mcp-server"
    return 1
}

# Validate Emacs availability
validate_emacs() {
    log "Checking Emacs..."
    
    if ! command_exists emacs; then
        error "Emacs not found. Install with: sudo apt install emacs"
        return 1
    fi
    
    success "Emacs found: $(emacs --version | head -1)"
    return 0
}

# Validate Node.js and dependencies
validate_nodejs() {
    log "Checking Node.js environment..."
    
    if ! command_exists node; then
        error "Node.js not found. Install from: https://nodejs.org/"
        return 1
    fi
    
    if ! command_exists npm; then
        error "npm not found. Install Node.js package manager"
        return 1
    fi
    
    success "Node.js found: $(node --version)"
    return 0
}

# Validate cafedelic scripts
validate_scripts() {
    log "Checking cafedelic scripts..."
    
    local scripts_dir="$CAFEDELIC_DIR/scripts"
    if [ ! -d "$scripts_dir" ]; then
        error "Scripts directory not found: $scripts_dir"
        return 1
    fi
    
    success "Scripts directory found"
    return 0
}
