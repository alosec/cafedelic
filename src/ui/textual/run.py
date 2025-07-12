#!/usr/bin/env python3
"""
Cafedelic Textual UI Entry Point
Launches the cafedelic intelligence platform TUI.
"""

import sys
import os

# Add project root to Python path to enable src.* imports
project_root = os.path.join(os.path.dirname(__file__), '..', '..', '..')
sys.path.insert(0, project_root)

from simple_chat import SimpleChatApp


def main():
    """Launch the cafedelic simple chat interface."""
    app = SimpleChatApp()
    app.run()


if __name__ == "__main__":
    main()