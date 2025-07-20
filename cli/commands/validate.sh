#!/bin/bash
#
# validate.sh - Validation commands for Cafedelic components
# Simple wrapper for component validation without GUI launch
#

set -euo pipefail

CAFEDELIC_DIR="${CAFEDELIC_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"

case "${1:-}" in
    textual)
        echo "🔍 Validating Textual TUI..."
        exec "$CAFEDELIC_DIR/src/ui/textual/scripts/validate-textual.sh"
        ;;
    debug)
        shift
        case "${1:-textual}" in
            textual)
                echo "🐛 Debug mode: Textual TUI..."
                cd "$CAFEDELIC_DIR/src/ui/textual"
                source venv/bin/activate
                exec python debug_tui.py
                ;;
            *)
                echo "Error: Unknown debug component '$1'"
                echo "Available: textual"
                exit 1
                ;;
        esac
        ;;
    --help|-h)
        cat << 'EOF'
Cafedelic Validation Commands

Usage: cafe validate <component> [options]

Components:
  textual        Validate Textual TUI without launching GUI
  debug textual  Debug Textual TUI showing all errors without GUI

Options:
  --help         Show this help message

Examples:
  cafe validate textual        # Check TUI for syntax/import/undefined function errors
  cafe validate debug textual  # Show all 7 errors comprehensively without GUI launch

EOF
        exit 0
        ;;
    "")
        echo "Error: No component specified"
        echo "Run 'cafe validate --help' for usage information"
        exit 1
        ;;
    *)
        echo "Error: Unknown component '$1'"
        echo "Run 'cafe validate --help' for available components"
        exit 1
        ;;
esac