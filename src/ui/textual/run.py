#!/usr/bin/env python3
"""
Cafedelic Textual UI Entry Point
Launches the cafedelic intelligence platform TUI.
"""

from hello_world import CafedelicApp


def main():
    """Launch the cafedelic textual UI."""
    app = CafedelicApp()
    app.run()


if __name__ == "__main__":
    main()