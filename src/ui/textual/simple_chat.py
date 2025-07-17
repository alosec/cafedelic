#!/usr/bin/env python3
"""
Simple Chat Interface for Cafedelic
Terminal-style chat interface inspired by Blink Shell experience.
"""

from textual.app import App, ComposeResult
from textual.containers import Vertical, ScrollableContainer
from textual.widgets import Input, Static
from textual.reactive import reactive
from datetime import datetime

# Using HTTP-based Claude Code filesystem integration
from session_manager import get_session_manager
import sys
import os

from cafe_ui.data.claude_code_data import get_sessions, get_projects
from cafe_ui.adapter import mcp_adapter


class SimpleChatApp(App):
    """A simple terminal-style chat application"""
    
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
        color: $success;
    }
    
    .system-message {
        color: $warning;
        text-style: italic;
    }
    
    .timestamp {
        color: $text-muted;
        text-style: dim;
    }
    """
    
    messages: reactive[list] = reactive(list)
    
    def compose(self) -> ComposeResult:
        """Compose the simple chat interface"""
        yield ScrollableContainer(id="messages")
        yield Input(
            placeholder="Type 'help' for commands, 'status' for system info, 'sessions' to list sessions",
            id="chat_input"
        )
    
    def on_mount(self) -> None:
        """Handle app mount"""
        # Initialize session manager (no database needed)
        self.session_manager = get_session_manager()
        
        # Load initial messages
        self.messages = []  # Start with empty messages - real chat functionality to be implemented
        self._render_messages()
        
        # Focus the input
        self.query_one("#chat_input", Input).focus()
        
        # Show minimal welcome message
        self._add_system_message("Cafedelic Intelligence Platform")
    
    def on_input_submitted(self, event: Input.Submitted) -> None:
        """Handle message submission"""
        if event.input.id == "chat_input":
            message = event.value.strip()
            if message:
                self._handle_user_message(message)
                event.input.value = ""
    
    def _handle_user_message(self, message: str) -> None:
        """Process user message and generate response"""
        # Add user message
        self._add_user_message(message)
        
        # Handle commands
        message_lower = message.lower()
        
        if message_lower == "help":
            self._show_help()
        elif message_lower == "status":
            self._show_status()
        elif message_lower == "projects":
            self._show_projects()
        elif message_lower == "sessions":
            self._show_sessions()
        elif message_lower.startswith("open "):
            session_id = message[5:].strip()  # Remove "open " prefix
            self._open_session(session_id)
        elif message_lower == "clear":
            self._clear_chat()
        elif message_lower.startswith("delegate "):
            task = message[9:]  # Remove "delegate " prefix
            self._delegate_task(task)
        elif message_lower.startswith("kill "):
            session_id = message[5:].strip()  # Remove "kill " prefix
            self._kill_session(session_id)
        else:
            # Echo response for now
            self._add_system_message(f"Unknown command: {message}. Type 'help' for available commands.")
    
    def _add_user_message(self, message: str) -> None:
        """Add a user message to the chat"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        self.messages.append({
            "type": "user",
            "content": message,
            "timestamp": timestamp
        })
        self._render_messages()
    
    def _add_system_message(self, message: str) -> None:
        """Add a system message to the chat"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        self.messages.append({
            "type": "system", 
            "content": message,
            "timestamp": timestamp
        })
        self._render_messages()
    
    def _render_messages(self) -> None:
        """Render all messages to the scroll container"""
        messages_container = self.query_one("#messages", ScrollableContainer)
        messages_container.remove_children()
        
        for msg in self.messages:
            timestamp = msg.get("timestamp", "")
            content = msg.get("content", "")
            msg_type = msg.get("type", "user")
            
            if msg_type == "user":
                css_class = "message user-message"
                prefix = ">"
            else:
                css_class = "message system-message"
                prefix = "*"
            
            message_text = f"[{timestamp}] {prefix} {content}"
            messages_container.mount(Static(message_text, classes=css_class))
        
        # Auto-scroll to bottom
        messages_container.scroll_end()
    
    def _show_help(self) -> None:
        """Show available commands"""
        help_text = """Available commands:
• help - Show this help
• status - Show current system status
• projects - List projects from Claude Code filesystem
• sessions - List Claude Code sessions
• open <id> - Open/resume session (e.g., 'open <session_id>')
• kill <id> - Terminate session (e.g., 'kill <session_id>')
• delegate <task> - Delegate task to AI
• clear - Clear chat history

Session Management:
• Use Ctrl+B then D to detach from session
• Use 'open <id>' to resume detached sessions
• Sessions run in background when detached

Data Source:
• All data comes directly from Claude Code's filesystem
• No database setup required - uses Claude's native storage"""
        self._add_system_message(help_text)
    
    def _show_status(self) -> None:
        """Show system status"""
        try:
            # Use HTTP-based data access instead of database
            projects = get_projects()
            sessions = get_sessions()
            
            # Check if MCP server is running
            health_check = mcp_adapter.get_health()
            server_status = "Running" if health_check.get("status") == "ok" else "Not Running"
            
            running_sessions = self.session_manager.list_running_sessions()
            
            status_text = f"""System Status:
• Cafedelic Server: {server_status}
• Data Source: Claude Code Filesystem
• Projects: {len(projects)}
• Sessions: {len(sessions)}
• Running Sessions: {len(running_sessions)}"""
                
        except Exception as e:
            status_text = f"Error getting status: {e}"
            
        self._add_system_message(status_text)
    
    def _show_projects(self) -> None:
        """Show all projects"""
        try:
            # Use HTTP-based data access instead of database
            projects = get_projects()
            if not projects:
                self._add_system_message("No projects found.")
                return
            
            projects_text = "Projects:\n"
            for project in projects:
                # HTTP response structure - adapt field names
                project_name = project.get('name', 'Unknown')
                project_path = project.get('path', 'Unknown')
                project_status = project.get('status', 'unknown')
                project_sessions = project.get('sessions', [])
                activity_level = project.get('activity_level', 0)
                
                # Convert activity level to indicator
                activity_indicator = ["○○○", "●○○", "●●○", "●●●"][min(activity_level, 3)]
                
                projects_text += f"• {project_name} [{project_status}] {activity_indicator}\n"
                projects_text += f"  Sessions: {len(project_sessions)} | Path: {project_path}\n"
            
            self._add_system_message(projects_text.strip())
            
        except Exception as e:
            self._add_system_message(f"Error getting projects: {e}")
    
    def _show_sessions(self) -> None:
        """Show active sessions"""
        try:
            # Use HTTP-based data access instead of database
            sessions = get_sessions()
            if not sessions:
                self._add_system_message("No sessions found.")
                return
            
            sessions_text = "Sessions:\n"
            for session in sessions:
                # HTTP response structure - adapt field names
                session_id = session.get('id', 'unknown')
                session_name = session.get('name', 'Unnamed')
                session_status = session.get('status', 'unknown')
                project_name = session.get('project', 'Unknown')
                task_description = session.get('task', 'No task')
                
                # Get tmux status if session_manager supports it
                tmux_status = "❓ Status unknown"
                if hasattr(self, 'session_manager') and hasattr(self.session_manager, 'get_session_info'):
                    tmux_info = self.session_manager.get_session_info(session_id)
                    tmux_status = "🔴 Running" if tmux_info and tmux_info.is_running else "⚫ Not running"
                
                status_emoji = {
                    'active': '🟢',
                    'available': '⚪',
                    'detached': '🟡',
                    'stuck': '🔴'
                }.get(session_status, '❓')
                
                last_activity = session.get('last_activity', 'Never')
                if last_activity != 'Never' and len(last_activity) > 19:
                    last_activity = last_activity[:19]
                
                sessions_text += f"• {session_id}: {session_name} {status_emoji} {session_status}\n"
                sessions_text += f"  Project: {project_name}\n"
                sessions_text += f"  Task: {task_description}\n"
                sessions_text += f"  Tmux: {tmux_status} | Last: {last_activity}\n"
                sessions_text += f"  Use 'open {session_id}' to start/resume\n"
            
            self._add_system_message(sessions_text.strip())
            
        except Exception as e:
            self._add_system_message(f"Error getting sessions: {e}")
    
    def _open_session(self, session_id: str) -> None:
        """Open/resume a Claude Code session"""
        try:
            # Use HTTP-based data access to validate session exists
            sessions = get_sessions()
            session = None
            for s in sessions:
                if s.get('id') == session_id:
                    session = s
                    break
            
            if not session:
                self._add_system_message(f"Session '{session_id}' not found. Use 'sessions' to see available sessions.")
                return
            
            session_name = session.get('name', 'Unknown')
            self._add_system_message(f"Opening session {session_id} ({session_name})...")
            
            # Check if session is already running
            tmux_info = self.session_manager.get_session_info(session_id)
            if tmux_info and tmux_info.is_running:
                self._add_system_message(f"Session is already running. Attaching to tmux session...")
                success = self.session_manager.attach_session(session_id)
            else:
                self._add_system_message(f"Starting new tmux session for {session_name}...")
                success = self.session_manager.open_claude_session(session_id, resume=True)
                if success:
                    self._add_system_message(f"Session started. Attaching...")
                    success = self.session_manager.attach_session(session_id)
            
            if success:
                self._add_system_message(f"✓ Attached to session {session_id}")
                self._add_system_message("Use Ctrl+B then D to detach and return to Cafedelic")
            else:
                self._add_system_message(f"✗ Failed to open session {session_id}")
                
        except Exception as e:
            self._add_system_message(f"Error opening session: {e}")
    
    def _kill_session(self, session_id: str) -> None:
        """Kill a session completely"""
        try:
            # Use HTTP-based data access to validate session exists
            sessions = get_sessions()
            session = None
            for s in sessions:
                if s.get('id') == session_id:
                    session = s
                    break
            
            if not session:
                self._add_system_message(f"Session '{session_id}' not found.")
                return
            
            session_name = session.get('name', 'Unknown')
            self._add_system_message(f"Killing session {session_id} ({session_name})...")
            
            success = self.session_manager.kill_session(session_id)
            if success:
                self._add_system_message(f"✓ Session {session_id} terminated")
            else:
                self._add_system_message(f"✗ Failed to kill session {session_id} (may not be running)")
                
        except Exception as e:
            self._add_system_message(f"Error killing session: {e}")
    
    def _delegate_task(self, task: str) -> None:
        """Delegate task to AI system"""
        if not task:
            self._add_system_message("Please specify a task to delegate")
            return
        
        try:
            # Use HTTP-based data access to find available sessions
            sessions = get_sessions()
            available_sessions = [s for s in sessions if s.get('status') == 'available']
            
            if not available_sessions:
                self._add_system_message("No available sessions found. All sessions are busy or stuck.")
                return
            
            # Pick first available session for now
            target_session = available_sessions[0]
            session_id = target_session.get('id', 'unknown')
            session_name = target_session.get('name', 'Unknown')
            
            self._add_system_message(f"Delegating task: '{task}'")
            self._add_system_message(f"→ Assigned to session {session_id} ({session_name})")
            self._add_system_message(f"→ Use 'open {session_id}' to start working on this task")
            
            # Note: Activity logging removed as it was database-specific
            # Task delegation is now a simple session recommendation
            
        except Exception as e:
            self._add_system_message(f"Error delegating task: {e}")
    
    def _clear_chat(self) -> None:
        """Clear chat history"""
        self.messages = []
        self._render_messages()
        self._add_system_message("Chat cleared")


if __name__ == "__main__":
    app = SimpleChatApp()
    app.run()