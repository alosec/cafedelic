#!/bin/bash
#
# debug.sh - Debug commands for Cafedelic components
# Comprehensive error capture and analysis without GUI launch
#

set -euo pipefail

CAFEDELIC_DIR="${CAFEDELIC_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"

case "${1:-}" in
    textual)
        echo "🐛 Debugging Textual TUI..."
        echo "   Capturing all errors without GUI launch..."
        echo
        
        # Change to TUI directory and activate environment
        cd "$CAFEDELIC_DIR/src/ui/textual"
        
        # Check for virtual environment
        if [[ ! -d "venv" ]]; then
            echo "❌ Virtual environment not found"
            echo "   Run: cafe validate textual (to set up environment)"
            exit 1
        fi
        
        # Activate and run debug
        source venv/bin/activate
        exec python debug_tui.py
        ;;
    --help|-h)
        cat << 'EOF'
Cafedelic Debug Commands

Usage: cafe debug <component> [options]

Components:
  textual        Debug Textual TUI showing all errors without GUI launch

Options:
  --help         Show this help message

Examples:
  cafe debug textual          # Show all 7 TUI errors comprehensively
  
Note:
  Debug mode provides comprehensive error analysis including:
  - Composition errors during widget creation
  - Runtime mounting issues 
  - Import and dependency problems
  - Textual framework integration issues

EOF
        exit 0
        ;;
    "")
        echo "Error: No component specified"
        echo "Run 'cafe debug --help' for usage information"
        exit 1
        ;;
    *)
        echo "Error: Unknown component '$1'"
        echo "Run 'cafe debug --help' for available components"
        exit 1
        ;;
esac