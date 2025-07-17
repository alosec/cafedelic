#!/usr/bin/env python3
"""
Minimal Chat Interface for Cafedelic - HTTP Integration Test
Simple interface to test Claude Code filesystem integration via HTTP API.
"""

from textual.app import App, ComposeResult
from textual.containers import Vertical, ScrollableContainer
from textual.widgets import Input, Static
from textual.reactive import reactive
from datetime import datetime

# Using HTTP-based Claude Code filesystem integration
from cafe_ui.data.claude_code_data import get_sessions, get_projects
from cafe_ui.adapter import mcp_adapter


class MinimalChatApp(App):
    """Minimal chat app to test HTTP integration"""
    
    CSS = """
    Screen {
        background: $surface;
    }
    
    #messages {
        height: 1fr;
        border: none;
        padding: 1;
        background: $surface;
    }
    
    #chat_input {
        height: 3;
        border: solid $primary;
        margin: 0;
    }
    
    .message {
        margin: 0 0 1 0;
        color: $text;
    }
    
    .user-message {
        color: $accent;
    }
    
    .system-message {
        color: $warning;
    }
    """
    
    def compose(self) -> ComposeResult:
        """Compose the app"""
        yield Vertical(
            ScrollableContainer(
                id="messages"
            ),
            Input(
                placeholder="Type 'help', 'status', 'projects', or 'sessions'",
                id="chat_input"
            )
        )
    
    def on_mount(self) -> None:
        """Handle app mount"""
        # Load initial messages
        self.messages = []
        self._render_messages()
        
        # Focus the input
        self.query_one("#chat_input", Input).focus()
        
        # Show welcome message
        self._add_system_message("Cafedelic HTTP Integration Test")
        self._add_system_message("Commands: help, status, projects, sessions")
    
    def on_input_submitted(self, event: Input.Submitted) -> None:
        """Handle message submission"""
        if event.input.id == "chat_input":
            message = event.input.value.strip()
            if message:
                self._add_user_message(message)
                self._handle_command(message)
                event.input.value = ""
    
    def _handle_command(self, message: str) -> None:
        """Handle command input"""
        parts = message.lower().split()
        if not parts:
            return
            
        command = parts[0]
        
        if command == "help":
            self._handle_help()
        elif command == "status":
            self._handle_status()
        elif command == "projects":
            self._handle_projects()
        elif command == "sessions":
            self._handle_sessions()
        else:
            self._add_system_message(f"Unknown command: {command}")
    
    def _handle_help(self) -> None:
        """Show help"""
        help_text = """Available Commands:
• help - Show this help message
• status - Show system status
• projects - List all projects from Claude Code
• sessions - List all sessions from Claude Code"""
        self._add_system_message(help_text)
    
    def _handle_status(self) -> None:
        """Show system status"""
        try:
            # Test server health
            health_check = mcp_adapter.get_health()
            server_status = "Running" if health_check.get("status") == "ok" else "Not Running"
            
            # Get data from Claude Code filesystem via HTTP
            projects = get_projects()
            sessions = get_sessions()
            
            status_text = f"""System Status:
• HTTP Server: {server_status}
• Data Source: Claude Code Filesystem
• Projects Found: {len(projects)}
• Sessions Found: {len(sessions)}"""
                
        except Exception as e:
            status_text = f"Error getting status: {str(e)}"
            
        self._add_system_message(status_text)
    
    def _handle_projects(self) -> None:
        """Show all projects"""
        try:
            projects = get_projects()
            if not projects:
                self._add_system_message("No projects found in Claude Code filesystem.")
                return
            
            projects_text = f"Found {len(projects)} projects:\n"
            for project in projects[:5]:  # Show first 5
                name = project.get('name', 'Unknown')
                path = project.get('path', 'Unknown')
                sessions = project.get('sessions', [])
                projects_text += f"• {name} ({path}) - {len(sessions)} sessions\n"
            
            if len(projects) > 5:
                projects_text += f"... and {len(projects) - 5} more"
            
            self._add_system_message(projects_text)
                
        except Exception as e:
            self._add_system_message(f"Error getting projects: {str(e)}")
    
    def _handle_sessions(self) -> None:
        """Show all sessions"""
        try:
            sessions = get_sessions()
            if not sessions:
                self._add_system_message("No sessions found in Claude Code filesystem.")
                return
            
            sessions_text = f"Found {len(sessions)} sessions:\n"
            for session in sessions[:5]:  # Show first 5
                session_id = session.get('id', 'Unknown')
                name = session.get('name', 'Unknown')
                project = session.get('project', 'Unknown')
                status = session.get('status', 'Unknown')
                sessions_text += f"• {session_id}: {name} ({project}) - {status}\n"
            
            if len(sessions) > 5:
                sessions_text += f"... and {len(sessions) - 5} more"
            
            self._add_system_message(sessions_text)
                
        except Exception as e:
            self._add_system_message(f"Error getting sessions: {str(e)}")
    
    def _add_user_message(self, message: str) -> None:
        """Add user message to chat"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        self.messages.append(f"[{timestamp}] You: {message}")
        self._render_messages()
    
    def _add_system_message(self, message: str) -> None:
        """Add system message to chat"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        self.messages.append(f"[{timestamp}] * {message}")
        self._render_messages()
    
    def _render_messages(self) -> None:
        """Render all messages"""
        container = self.query_one("#messages", ScrollableContainer)
        container.remove_children()
        
        for message in self.messages:
            if message.startswith("[") and "] You:" in message:
                widget = Static(message, classes="message user-message")
            else:
                widget = Static(message, classes="message system-message")
            container.mount(widget)
        
        # Auto-scroll to bottom
        if container.children:
            container.scroll_end()


if __name__ == "__main__":
    app = MinimalChatApp()
    app.run()