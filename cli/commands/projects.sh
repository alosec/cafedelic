#!/bin/bash
# Projects command for Cafedelic CLI
# Usage: cafe projects <subcommand> [options]

set -euo pipefail

# Source common utilities
source "$(dirname "$0")/lib/common.sh"

# Check if Python virtual environment exists
check_venv() {
    local venv_dir="$CAFEDELIC_DIR/src/ui/textual/venv"
    if [[ ! -d "$venv_dir" ]]; then
        echo "Error: Python virtual environment not found at $venv_dir"
        echo "Please run 'cafe init' first to set up the environment"
        exit 1
    fi
}

# Run Python command with proper environment
run_python() {
    local script="$1"
    cd "$CAFEDELIC_DIR/src/ui/textual"
    source venv/bin/activate
    python3 -c "$script"
}

# Show help
show_help() {
    cat << 'EOF'
Projects command for Cafedelic CLI

Usage: cafe projects <subcommand> [options]

Subcommands:
  list    List projects from database
  scan    Scan filesystem for projects

Options for 'list':
  --status <status>    Filter by project status (active, idle, issues)
  --has-sessions       Only show projects with sessions
  --format <format>    Output format (table, json, csv) [default: table]

Options for 'scan':
  <path>               Path to scan [default: current directory]
  --format <format>    Output format (table, json, csv) [default: table]
  --recursive          Scan subdirectories recursively

Examples:
  cafe projects list                    # List all projects
  cafe projects list --status active   # List active projects
  cafe projects list --has-sessions    # List projects with sessions
  cafe projects scan                    # Scan current directory
  cafe projects scan ~/code             # Scan ~/code directory
  cafe projects scan ~/code --recursive # Scan ~/code recursively
EOF
}

# Parse common options
parse_options() {
    STATUS=""
    HAS_SESSIONS=""
    FORMAT="table"
    PATH=""
    RECURSIVE=""
    
    # Handle positional path argument for scan
    if [[ "${1:-}" != --* ]] && [[ "${1:-}" != "" ]] && [[ "$SUBCOMMAND" == "scan" ]]; then
        PATH="$1"
        shift
    fi
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --status)
                STATUS="$2"
                shift 2
                ;;
            --has-sessions)
                HAS_SESSIONS="true"
                shift
                ;;
            --format)
                FORMAT="$2"
                shift 2
                ;;
            --recursive)
                RECURSIVE="true"
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                echo "Run 'cafe projects --help' for usage information"
                exit 1
                ;;
        esac
    done
}

# List projects from database
list_projects() {
    local script=$(cat << 'EOF'
import sys
sys.path.insert(0, '/home/alex/code/cafedelic/src')

from database.cli_interface import get_cli_interface
import os

def main():
    cli = get_cli_interface()
    
    # Get filter options from environment
    status = os.environ.get('STATUS', '')
    has_sessions = os.environ.get('HAS_SESSIONS', '') == 'true'
    format_type = os.environ.get('FORMAT', 'table')
    
    try:
        result = cli.list_projects(
            status=status if status else None,
            has_sessions=has_sessions,
            format_type=format_type
        )
        print(result)
        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

if __name__ == "__main__":
    exit(main())
EOF
)
    
    # Set environment variables for the Python script
    export STATUS="$STATUS"
    export HAS_SESSIONS="$HAS_SESSIONS"
    export FORMAT="$FORMAT"
    
    run_python "$script"
}

# Scan filesystem for projects
scan_projects() {
    local script=$(cat << 'EOF'
import sys
sys.path.insert(0, '/home/alex/code/cafedelic/src')

from database.cli_interface import get_cli_interface
import os

def main():
    cli = get_cli_interface()
    
    # Get options from environment
    path = os.environ.get('PATH_SCAN', '')
    format_type = os.environ.get('FORMAT', 'table')
    recursive = os.environ.get('RECURSIVE', '') == 'true'
    
    try:
        result = cli.scan_projects(
            path=path if path else None,
            recursive=recursive,
            format_type=format_type
        )
        print(result)
        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

if __name__ == "__main__":
    exit(main())
EOF
)
    
    # Set environment variables for the Python script
    export PATH_SCAN="$PATH"
    export FORMAT="$FORMAT"
    export RECURSIVE="$RECURSIVE"
    
    run_python "$script"
}

# Main command handling
SUBCOMMAND="${1:-}"
case "$SUBCOMMAND" in
    list)
        check_venv
        shift
        parse_options "$@"
        list_projects
        ;;
    scan)
        check_venv
        shift
        parse_options "$@"
        scan_projects
        ;;
    --help|-h)
        show_help
        exit 0
        ;;
    "")
        echo "Missing subcommand. Available: list, scan"
        echo "Run 'cafe projects --help' for usage information"
        exit 1
        ;;
    *)
        echo "Unknown subcommand: $1"
        echo "Run 'cafe projects --help' for usage information"
        exit 1
        ;;
esac