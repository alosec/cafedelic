#!/usr/bin/env python3
"""
Cafedelic Task Delegation Platform
Textual TUI for async Claude Code orchestration and session management.
"""

from textual.app import App, ComposeResult
from textual.widgets import Header, Footer

from cafe_ui.screens.main_dashboard import MainDashboard


class CafedelicApp(App):
    """Cafedelic Task Delegation Platform - Main Application"""
    
    TITLE = "Cafedelic Task Delegation Platform"
    SUB_TITLE = "Async Claude Code Orchestration"
    
    BINDINGS = [
        ("ctrl+c", "quit", "Quit"),
        ("ctrl+q", "quit", "Quit"),
    ]
    
    def on_mount(self) -> None:
        """Handle app startup"""
        self.push_screen(MainDashboard())
    
    def compose(self) -> ComposeResult:
        """This will not be called since we push a screen immediately"""
        # This method is required but won't be used since we push MainDashboard immediately
        yield Header()
        yield Footer()


if __name__ == "__main__":
    app = CafedelicApp()
    app.run()