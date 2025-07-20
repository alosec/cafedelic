#!/usr/bin/env python3
"""
Cafedelic Navigation Platform
Textual TUI for async Claude Code orchestration and session management.
"""

from textual.app import App, ComposeResult, SystemCommand
from textual.widgets import Header, Footer, TabbedContent, TabPane
from textual.screen import Screen
from typing import Iterable

from cafe_ui.screens.project_navigator import ProjectNavigator
from cafe_ui.screens.session_navigator import SessionNavigator
from cafe_ui.components.quick_chat import QuickChatWidget


class CafedelicApp(App):
    """Cafedelic Navigation Platform - Main Application"""
    
    TITLE = "Cafedelic Navigation Platform"
    SUB_TITLE = "Project & Session Management"
    
    BINDINGS = [
        ("ctrl+c", "quit", "Quit"),
        ("ctrl+q", "quit", "Quit"),
        ("p", "show_projects", "Projects"),
        ("s", "show_sessions", "Sessions"),
        ("c", "show_chat", "Chat"),
        ("1", "show_projects", "Projects"),
        ("2", "show_sessions", "Sessions"),
        ("3", "show_chat", "Chat"),
        ("r", "refresh_all_data", "Refresh All"),
    ]
    
    def compose(self) -> ComposeResult:
        """Compose the main application with tabbed navigation"""
        yield Header()
        
        with TabbedContent(id="main_tabs"):
            with TabPane("Project Navigator", id="project_tab"):
                yield ProjectNavigator(id="project_navigator")
            
            with TabPane("Session Navigator", id="session_tab"):
                yield SessionNavigator(id="session_navigator")
            
            with TabPane("Quick Chat", id="chat_tab"):
                yield QuickChatWidget(id="quick_chat")
        
        yield Footer()

    def on_mount(self) -> None:
        """Handle app startup"""
        self.title = "Cafedelic - Project & Session Navigator"
        self.sub_title = "Manage projects and sessions with AI delegation"

    def get_system_commands(self, screen: Screen) -> Iterable[SystemCommand]:
        """Get custom commands for the command palette"""
        # Navigation commands
        yield SystemCommand(
            "Switch to Project Navigator",
            "Navigate to project management view",
            self.action_show_projects
        )
        
        yield SystemCommand(
            "Switch to Session Navigator", 
            "Navigate to session management view",
            self.action_show_sessions
        )
        
        yield SystemCommand(
            "Switch to Quick Chat",
            "Navigate to chat delegation interface",
            self.action_show_chat
        )
        
        # Data refresh commands
        yield SystemCommand(
            "Refresh All Data",
            "Refresh project and session data across all views",
            self.refresh_all_data
        )
        
        # Context-specific commands based on active tab
        active_tab = self.get_active_tab()
        
        if active_tab == "project_tab":
            yield SystemCommand(
                "Create New Project Session",
                "Create a new session for the selected project",
                self._create_new_project_session
            )
            
            yield SystemCommand(
                "Refresh Project Data",
                "Refresh data for the project navigator",
                self._refresh_project_data
            )
            
        elif active_tab == "session_tab":
            yield SystemCommand(
                "Create New Session",
                "Create a new session",
                self._create_new_session
            )
            
            yield SystemCommand(
                "Filter Active Sessions",
                "Show only active sessions",
                self._filter_active_sessions
            )
            
            yield SystemCommand(
                "Filter Stuck Sessions",
                "Show only stuck sessions that need attention",
                self._filter_stuck_sessions
            )
            
            yield SystemCommand(
                "Refresh Session Data",
                "Refresh data for the session navigator",
                self._refresh_session_data
            )
            
        elif active_tab == "chat_tab":
            yield SystemCommand(
                "Clear Chat History",
                "Clear the chat conversation history",
                self._clear_chat_history
            )

    def action_show_projects(self) -> None:
        """Switch to project navigator"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        tabs.active = "project_tab"

    def action_show_sessions(self) -> None:
        """Switch to session navigator"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        tabs.active = "session_tab"

    def action_show_chat(self) -> None:
        """Switch to chat interface"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        tabs.active = "chat_tab"

    def on_project_navigator_project_selected(self, event) -> None:
        """Handle project selection from project navigator"""
        # Could update other components or navigate to session view
        pass

    def on_project_navigator_session_requested(self, event) -> None:
        """Handle session view request from project navigator"""
        # Switch to session navigator and select the session
        self.action_show_sessions()
        session_navigator = self.query_one("#session_navigator", SessionNavigator)
        session_navigator.select_session(event.session_id)

    def on_project_navigator_chat_requested(self, event) -> None:
        """Handle chat request from project navigator"""
        # Switch to chat and pre-select session if provided
        self.action_show_chat()
        if hasattr(event, 'session_id') and event.session_id:
            chat_widget = self.query_one("#quick_chat", QuickChatWidget)
            # TODO: Pre-select session in chat widget

    def on_session_navigator_session_selected(self, event) -> None:
        """Handle session selection from session navigator"""
        # Could update other components
        pass

    def on_session_navigator_project_view_requested(self, event) -> None:
        """Handle project view request from session navigator"""
        # Switch to project navigator and select the project
        self.action_show_projects()
        project_navigator = self.query_one("#project_navigator", ProjectNavigator)
        project_navigator.set_project(event.project_name)

    def on_session_navigator_chat_requested(self, event) -> None:
        """Handle chat request from session navigator"""
        # Switch to chat and pre-select session if provided
        self.action_show_chat()
        if hasattr(event, 'session_id') and event.session_id:
            chat_widget = self.query_one("#quick_chat", QuickChatWidget)
            # TODO: Pre-select session in chat widget

    def get_active_tab(self) -> str:
        """Get the currently active tab"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        return tabs.active

    def refresh_all_data(self) -> None:
        """Refresh data in all navigators"""
        project_navigator = self.query_one("#project_navigator", ProjectNavigator)
        project_navigator.refresh_all()
        
        session_navigator = self.query_one("#session_navigator", SessionNavigator)
        session_navigator.refresh_all()

    # Command palette action methods
    def _create_new_project_session(self) -> None:
        """Create a new session for the selected project"""
        try:
            project_navigator = self.query_one("#project_navigator", ProjectNavigator)
            selected_project = project_navigator.get_selected_project()
            if selected_project:
                project_navigator.show_session_creation_dialog()
            else:
                self.notify("Please select a project first", severity="warning")
        except Exception as e:
            self.notify(f"Error creating session: {e}", severity="error")

    def _create_new_session(self) -> None:
        """Create a new session"""
        try:
            session_navigator = self.query_one("#session_navigator", SessionNavigator)
            session_navigator.show_session_creation_dialog()
        except Exception as e:
            self.notify(f"Error creating session: {e}", severity="error")

    def _filter_active_sessions(self) -> None:
        """Filter to show only active sessions"""
        try:
            session_navigator = self.query_one("#session_navigator", SessionNavigator)
            session_navigator.set_filter("active")
            self.notify("Filtered to active sessions")
        except Exception as e:
            self.notify(f"Error filtering sessions: {e}", severity="error")

    def _filter_stuck_sessions(self) -> None:
        """Filter to show only stuck sessions"""
        try:
            session_navigator = self.query_one("#session_navigator", SessionNavigator)
            session_navigator.set_filter("stuck")
            self.notify("Filtered to stuck sessions")
        except Exception as e:
            self.notify(f"Error filtering sessions: {e}", severity="error")

    def _refresh_project_data(self) -> None:
        """Refresh project navigator data"""
        try:
            project_navigator = self.query_one("#project_navigator", ProjectNavigator)
            project_navigator.refresh_all()
            self.notify("Project data refreshed")
        except Exception as e:
            self.notify(f"Error refreshing project data: {e}", severity="error")

    def _refresh_session_data(self) -> None:
        """Refresh session navigator data"""
        try:
            session_navigator = self.query_one("#session_navigator", SessionNavigator)
            session_navigator.refresh_all()
            self.notify("Session data refreshed")
        except Exception as e:
            self.notify(f"Error refreshing session data: {e}", severity="error")

    def _clear_chat_history(self) -> None:
        """Clear chat history"""
        try:
            chat_widget = self.query_one("#quick_chat", QuickChatWidget)
            if hasattr(chat_widget, 'clear_history'):
                chat_widget.clear_history()
                self.notify("Chat history cleared")
            else:
                self.notify("Chat clear not available", severity="warning")
        except Exception as e:
            self.notify(f"Error clearing chat: {e}", severity="error")


if __name__ == "__main__":
    app = CafedelicApp()
    app.run()