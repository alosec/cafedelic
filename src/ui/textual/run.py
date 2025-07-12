#!/usr/bin/env python3
"""
Cafedelic Textual UI Entry Point
Launches the cafedelic intelligence platform TUI.
"""

from simple_chat import SimpleChatApp


def main():
    """Launch the cafedelic simple chat interface."""
    app = SimpleChatApp()
    app.run()


if __name__ == "__main__":
    main()