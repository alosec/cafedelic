#!/bin/bash
# Cafedelic CLI - open command
# Launch UI interfaces

set -euo pipefail

# Load libraries
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Show usage information
show_usage() {
    cat << 'EOF'
cafe open - Launch UI interfaces

Usage: cafe open <interface>

Interfaces:
  textual   Launch Textual intelligence platform UI

Examples:
  cafe open textual   # Launch Python TUI for cafedelic intelligence

EOF
}

# Launch textual interface
launch_textual() {
    log "Launching Textual intelligence platform UI..."
    
    # Navigate to textual directory
    TEXTUAL_DIR="$CAFEDELIC_DIR/src/ui/textual"
    
    if [[ ! -d "$TEXTUAL_DIR" ]]; then
        error "Textual UI directory not found: $TEXTUAL_DIR"
        exit 1
    fi
    
    cd "$TEXTUAL_DIR"
    
    # Set up virtual environment if it doesn't exist
    VENV_DIR="$TEXTUAL_DIR/venv"
    if [[ ! -d "$VENV_DIR" ]]; then
        log "Creating Python virtual environment..."
        
        # Try python3.12 first, then fall back to python3
        if command -v python3.12 >/dev/null 2>&1; then
            PYTHON_CMD="python3.12"
        elif command -v python3 >/dev/null 2>&1; then
            PYTHON_CMD="python3"
        else
            error "Python 3 not found. Please install python3."
            exit 1
        fi
        
        "$PYTHON_CMD" -m venv "$VENV_DIR"
        success "Virtual environment created at $VENV_DIR"
    fi
    
    # Activate virtual environment
    source "$VENV_DIR/bin/activate"
    
    # Check if requirements are installed in venv
    if ! python -c "import textual" 2>/dev/null; then
        warn "Installing Textual dependencies in virtual environment..."
        if [[ -f "requirements.txt" ]]; then
            pip install -r requirements.txt
        else
            pip install textual
        fi
        success "Dependencies installed successfully"
    fi
    
    # Launch the app
    success "Starting Textual UI at $TEXTUAL_DIR"
    python run.py
}

# Main function
main() {
    case "${1:-}" in
        textual)
            launch_textual
            ;;
        --help|-h|"")
            show_usage
            exit 0
            ;;
        *)
            error "Unknown interface: ${1:-}"
            echo
            show_usage
            exit 1
            ;;
    esac
}

# Execute main function with all arguments
main "$@"