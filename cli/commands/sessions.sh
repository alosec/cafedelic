#!/bin/bash
# Sessions command for Cafedelic CLI
# Usage: cafe sessions <subcommand> [options]

set -euo pipefail

# Source common utilities
source "$(dirname "$0")/lib/common.sh"

# Show help
show_help() {
    cat << 'EOF'
Sessions command for Cafedelic CLI

Usage: cafe sessions <subcommand> [options]

Subcommands:
  list    List sessions from database
  load    Load Claude Code sessions into database

Options for 'list':
  --project <name>     Filter by project name
  --format <format>    Output format (table, json) [default: table]

Options for 'load':
  --path <path>        Path to scan [default: ~/.claude/projects]

Examples:
  cafe sessions list                    # List all sessions
  cafe sessions list --project webapp  # List sessions for webapp project
  cafe sessions load                   # Load sessions from Claude Code
EOF
}

# Parse common options
parse_options() {
    PROJECT=""
    FORMAT="table"
    PATH=""
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --project)
                PROJECT="$2"
                shift 2
                ;;
            --format)
                FORMAT="$2"
                shift 2
                ;;
            --path)
                PATH="$2"
                shift 2
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                echo "Run 'cafe sessions --help' for usage information"
                exit 1
                ;;
        esac
    done
}

# List sessions from database using Python
list_sessions() {
    cd "$CAFEDELIC_DIR"
    
    local python_script="
from src.database.session_db import get_database
import json

db = get_database()
sessions = db.get_sessions()

if '$FORMAT' == 'json':
    session_data = []
    for session in sessions:
        session_data.append({
            'id': session.short_id,
            'name': session.name,
            'project': session.project_name,
            'status': session.status,
            'task': session.task_description,
            'uuid': session.claude_session_uuid
        })
    print(json.dumps(session_data, indent=2))
else:
    # Table format
    print('ID    NAME                PROJECT       STATUS     TASK')
    print('---   ----------------    -----------   --------   ----')
    for session in sessions:
        print(f'{session.short_id:<5} {session.name:<18} {session.project_name:<12} {session.status:<9} {session.task_description[:30]}')
"
    
    # Use Python from textual venv that has the project modules
    TEXTUAL_VENV="$CAFEDELIC_DIR/src/ui/textual/venv/bin/python"
    if [[ -x "$TEXTUAL_VENV" ]]; then
        "$TEXTUAL_VENV" -c "$python_script"
    else
        echo "Error: Virtual environment not found at $TEXTUAL_VENV"
        echo "Run 'cafe open textual' first to set up the environment"
        exit 1
    fi
}

# Load Claude Code sessions into database
load_sessions() {
    cd "$CAFEDELIC_DIR"
    
    local python_script="
from src.database.claude_discovery import get_claude_discovery
from src.database.session_db import get_database
import os

# Get discovery service
discovery = get_claude_discovery()
db = get_database()

print('Loading Claude Code sessions...')

# Discover sessions
sessions = discovery.find_all_sessions()
print(f'Found {len(sessions)} Claude Code sessions')

# Group sessions by project
projects_map = {}
for session in sessions:
    project_path = session.project_path
    if project_path not in projects_map:
        projects_map[project_path] = []
    projects_map[project_path].append(session)

# Create projects and sessions
for project_path, project_sessions in projects_map.items():
    # Create project
    project_name = os.path.basename(project_path)
    normalized_path = project_path.replace('/', '-').lstrip('-')
    
    try:
        project_id = db.create_project(project_name, project_path, normalized_path)
        print(f'Created project {project_id}: {project_name}')
        
        # Create sessions for this project
        for session in project_sessions:
            try:
                session_id = db.create_session_from_claude_data(session, project_id)
                print(f'  Created session {session_id}: {session.conversation_turns} messages')
            except Exception as e:
                print(f'  Error creating session: {e}')
                
    except Exception as e:
        print(f'Error creating project {project_name}: {e}')

print('Loading complete')
"
    
    # Use Python from textual venv that has the project modules
    TEXTUAL_VENV="$CAFEDELIC_DIR/src/ui/textual/venv/bin/python"
    if [[ -x "$TEXTUAL_VENV" ]]; then
        "$TEXTUAL_VENV" -c "$python_script"
    else
        echo "Error: Virtual environment not found at $TEXTUAL_VENV"
        echo "Run 'cafe open textual' first to set up the environment"
        exit 1
    fi
}

# Main command handling
case "${1:-}" in
    list)
        shift
        parse_options "$@"
        list_sessions
        ;;
    load)
        shift
        parse_options "$@"
        load_sessions
        ;;
    --help|-h)
        show_help
        exit 0
        ;;
    "")
        echo "Missing subcommand. Available: list, load"
        echo "Run 'cafe sessions --help' for usage information"
        exit 1
        ;;
    *)
        echo "Unknown subcommand: $1"
        echo "Run 'cafe sessions --help' for usage information"
        exit 1
        ;;
esac