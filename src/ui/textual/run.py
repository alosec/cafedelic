#!/usr/bin/env python3
"""
Cafedelic Textual UI Entry Point
Launches the cafedelic navigation platform TUI.
"""

import sys
import os
import subprocess

# Add project root to Python path to enable src.* imports
project_root = os.path.join(os.path.dirname(__file__), '..', '..', '..')
sys.path.insert(0, project_root)

from cafedelic_app import CafedelicApp


def main():
    """Launch the cafedelic navigation platform."""
    # Check for validation mode
    if len(sys.argv) > 1 and sys.argv[1] == "--validate":
        print("Running validation mode...")
        result = subprocess.run([sys.executable, "validate_tui.py"], 
                              capture_output=False, 
                              text=True)
        sys.exit(result.returncode)
    
    # Check for debug mode
    if len(sys.argv) > 1 and sys.argv[1] == "--debug":
        print("Running debug mode...")
        result = subprocess.run([sys.executable, "debug_tui.py"], 
                              capture_output=False, 
                              text=True)
        sys.exit(result.returncode)
    
    # Normal GUI launch
    app = CafedelicApp()
    app.run()


if __name__ == "__main__":
    main()