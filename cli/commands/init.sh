#!/bin/bash
# Cafedelic CLI - init command
# Validate server and dependencies

set -euo pipefail

# Load libraries
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"
source "$SCRIPT_DIR/lib/validation.sh"

# Main validation function
main() {
    log "Cafedelic system validation starting..."
    echo
    
    local all_valid=true
    
    # Tmux validation (required)
    if ! validate_tmux; then
        all_valid=false
    fi
    echo
    
    # Emacs validation (required)
    if ! validate_emacs; then
        all_valid=false
    fi
    echo
    
    # Node.js validation (required)
    if ! validate_nodejs; then
        all_valid=false
    fi
    echo
    
    # Scripts validation (required)
    if ! validate_scripts; then
        all_valid=false
    fi
    echo
    
    # MCP server validation (optional but recommended)
    validate_mcp_server || warn "MCP server validation failed (non-critical)"
    echo
    
    if [ "$all_valid" = true ]; then
        success "All system validations passed!"
        success "Cafedelic is ready to use"
        return 0
    else
        error "System validation failed. Please fix the errors above."
        return 1
    fi
}

main "$@"
