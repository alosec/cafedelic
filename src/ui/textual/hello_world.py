#!/usr/bin/env python3
"""
Cafedelic Textual UI - Hello World
A basic Textual TUI app demonstrating the foundation for cafedelic's intelligence platform UI.
"""

from textual.app import App, ComposeResult
from textual.widgets import Header, Footer, Static
from textual.containers import Vertical


class CafedelicApp(App):
    """A simple Textual app showcasing the cafedelic intelligence platform."""
    
    CSS = """
    #content {
        background: $surface;
        border: thick $background 80%;
        padding: 1;
        margin: 1;
    }
    
    Static {
        text-align: center;
        margin: 1;
    }
    """
    
    BINDINGS = [
        ("d", "toggle_dark", "Toggle dark mode"),
        ("q", "quit", "Quit")
    ]

    def compose(self) -> ComposeResult:
        """Create child widgets for the app."""
        yield Header()
        with Vertical(id="content"):
            yield Static("🚀 Cafedelic Intelligence Platform", classes="title")
            yield Static("Hello, Textual World!", classes="greeting") 
            yield Static("Intelligence layer for AI-assisted development", classes="subtitle")
            yield Static("")
            yield Static("Press 'd' to toggle dark mode, 'q' to quit", classes="instructions")
        yield Footer()

    def action_toggle_dark(self) -> None:
        """Toggle between light and dark themes."""
        self.theme = (
            "textual-dark" if self.theme == "textual-light" else "textual-light"
        )


if __name__ == "__main__":
    app = CafedelicApp()
    app.run()