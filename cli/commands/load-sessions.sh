#!/bin/bash
# Load discovered Claude Code sessions into database
# Usage: cafe load-sessions [--verbose] [--dry-run]

set -euo pipefail

# Source common utilities
source "$(dirname "$0")/lib/common.sh"

VERBOSE=false
DRY_RUN=false

# Parse command line options
while [[ $# -gt 0 ]]; do
    case $1 in
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --dry-run|-n)
            DRY_RUN=true
            shift
            ;;
        --help|-h)
            cat << 'EOF'
Load discovered Claude Code sessions into database

Usage: cafe load-sessions [options]

Options:
  --verbose, -v    Show detailed output and debug information
  --dry-run, -n    Show what would be loaded without making changes
  --help, -h       Show this help message

Examples:
  cafe load-sessions           # Load sessions quietly
  cafe load-sessions --verbose # Load with detailed output
  cafe load-sessions --dry-run # Preview what would be loaded

EOF
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Run 'cafe load-sessions --help' for usage information"
            exit 1
            ;;
    esac
done

# Check if Python virtual environment exists
VENV_DIR="$CAFEDELIC_DIR/src/ui/textual/venv"
if [[ ! -d "$VENV_DIR" ]]; then
    echo "Error: Python virtual environment not found at $VENV_DIR"
    echo "Please run 'cafe init' first to set up the environment"
    exit 1
fi

# Activate virtual environment and run session loader
cd "$CAFEDELIC_DIR/src/ui/textual"

if [[ "$VERBOSE" == "true" ]]; then
    echo "Activating Python virtual environment..."
    echo "Working directory: $(pwd)"
fi

# Create Python script for session loading
PYTHON_SCRIPT=$(cat << 'EOF'
import sys
import os

# Add src to Python path for imports
sys.path.insert(0, '/home/alex/code/cafedelic/src')

from database.session_loader import get_session_loader
from database.claude_discovery import get_claude_discovery

def main():
    try:
        print("Cafedelic Session Loader")
        print("=" * 40)
        
        loader = get_session_loader()
        discovery = get_claude_discovery()
        
        # Get current state
        summary = loader.get_session_loading_summary()
        
        print(f"Claude sessions discovered: {summary['claude_sessions_discovered']}")
        print(f"Database sessions from Claude: {summary['database_sessions_from_claude']}")
        print(f"Sessions not in database: {summary['claude_sessions_not_in_db']}")
        print(f"Active Claude sessions: {summary['active_claude_sessions']}")
        print()
        
        if summary['claude_sessions_not_in_db'] == 0:
            print("✓ All Claude sessions are already loaded in database")
            return 0
        
        dry_run = os.environ.get('DRY_RUN', 'false') == 'true'
        verbose = os.environ.get('VERBOSE', 'false') == 'true'
        
        if dry_run:
            print(f"DRY RUN: Would load {summary['claude_sessions_not_in_db']} sessions")
            
            # Show which projects would be affected
            claude_sessions = discovery.find_all_sessions()
            projects_by_path = {}
            for session in claude_sessions:
                if session.project_path not in projects_by_path:
                    projects_by_path[session.project_path] = []
                projects_by_path[session.project_path].append(session)
            
            print("\nProjects that would be affected:")
            for project_path, sessions in projects_by_path.items():
                exists = os.path.exists(project_path)
                status = "✓" if exists else "✗ (missing)"
                print(f"  {status} {project_path} ({len(sessions)} sessions)")
            
            return 0
        
        print(f"Loading {summary['claude_sessions_not_in_db']} sessions...")
        
        # Load the sessions
        results = loader.load_discovered_sessions()
        
        # Report results
        print(f"\n✓ Session loading complete:")
        print(f"  • Discovered: {results['discovered']} sessions")
        print(f"  • Loaded: {results['loaded']} new sessions")
        print(f"  • Skipped: {results['skipped']} (already existed)")
        
        if results['errors'] > 0:
            print(f"  • Errors: {results['errors']} sessions failed to load")
            if verbose:
                print("\nRun with --verbose to see error details in the application")
        
        if results['loaded'] > 0:
            print(f"\nUse 'cafe open textual' then 'sessions' to see all loaded sessions")
        
        return 0 if results['errors'] == 0 else 1
        
    except Exception as e:
        print(f"Error: {e}")
        if verbose:
            import traceback
            traceback.print_exc()
        return 1

if __name__ == "__main__":
    exit(main())
EOF
)

# Set environment variables for the Python script
export DRY_RUN="$DRY_RUN"
export VERBOSE="$VERBOSE"

# Run the Python script
if [[ "$VERBOSE" == "true" ]]; then
    echo "Running session loader..."
    echo
fi

source venv/bin/activate
python3 -c "$PYTHON_SCRIPT"
exit_code=$?

if [[ "$VERBOSE" == "true" ]]; then
    echo
    echo "Session loading completed with exit code: $exit_code"
fi

exit $exit_code