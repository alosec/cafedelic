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

from cafe_ui.data.mock_data import get_chat_messages
from session_manager import get_session_manager
import sys
import os

from src.database.session_db import get_database
from src.database.claude_discovery import get_claude_discovery
from src.database.project_discovery import get_project_discovery
from src.database.session_loader import get_session_loader


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
            placeholder="Type your message... (or try 'help', 'status', 'sessions')",
            id="chat_input"
        )
    
    def on_mount(self) -> None:
        """Handle app mount"""
        # Initialize database and managers
        self.db = get_database()
        self.session_manager = get_session_manager()
        self.claude_discovery = get_claude_discovery()
        self.project_discovery = get_project_discovery()
        self.session_loader = get_session_loader()
        
        # Load initial messages
        self.messages = get_chat_messages()
        self._render_messages()
        
        # Focus the input
        self.query_one("#chat_input", Input).focus()
        
        # Show minimal welcome message
        if self.db.is_initialized():
            self._add_system_message("Cafedelic Intelligence Platform")
        else:
            self._add_system_message("Cafedelic - Database not initialized. Type 'init' to begin.")
    
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
        elif message_lower.startswith("scan "):
            path = message[5:].strip()  # Remove "scan " prefix
            self._scan_path(path)
        elif message_lower == "suggest":
            self._suggest_paths()
        elif message_lower == "status":
            self._show_status()
        elif message_lower == "projects":
            self._show_projects()
        elif message_lower == "sessions":
            self._show_sessions()
        elif message_lower == "load sessions":
            self._load_sessions()
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
• suggest - Suggest project paths to scan
• scan <path> - Scan directory for projects and add to database
• projects - List tracked projects from database
• sessions - List Claude Code sessions
• load sessions - Load discovered Claude Code sessions into database
• open <id> - Open/resume session (e.g., 'open s1')
• kill <id> - Terminate session (e.g., 'kill s1')
• delegate <task> - Delegate task to AI
• clear - Clear chat history

Session Management:
• Use Ctrl+B then D to detach from session
• Use 'open <id>' to resume detached sessions
• Sessions run in background when detached

Discovery Examples:
• scan ~/code - Find and add projects in ~/code directory
• scan /home/alex/projects - Scan specific project directory"""
        self._add_system_message(help_text)
    
    def _scan_path(self, path: str) -> None:
        """Scan a directory path for projects"""
        try:
            if not path:
                self._add_system_message("Please specify a path to scan (e.g., 'scan ~/code')")
                return
            
            self._add_system_message(f"Scanning {path} for projects...")
            
            # Validate path first
            validation = self.project_discovery.validate_project_path(path)
            if not validation['exists']:
                self._add_system_message(f"Path does not exist: {path}")
                return
            
            if not validation['is_directory']:
                self._add_system_message(f"Path is not a directory: {path}")
                return
            
            # Scan for projects
            discovered = self.project_discovery.scan_directory_for_projects(path, max_depth=2)
            
            if not discovered:
                self._add_system_message(f"No projects found in {path}")
                return
            
            self._add_system_message(f"Found {len(discovered)} projects:")
            
            for project in discovered[:10]:  # Show first 10
                indicators = []
                if project.has_git:
                    indicators.append("Git")
                if project.has_claude_sessions:
                    indicators.append(f"Claude({project.claude_project.session_count})")
                
                indicator_text = f" [{', '.join(indicators)}]" if indicators else ""
                self._add_system_message(f"• {project.name}{indicator_text}")
                self._add_system_message(f"  {project.path}")
            
            if len(discovered) > 10:
                self._add_system_message(f"... and {len(discovered) - 10} more projects")
            
            # Add discovered projects to database
            added_count = 0
            for project in discovered:
                try:
                    # Check if project already exists in database
                    existing_projects = self.db.get_projects()
                    if any(p.path == project.path for p in existing_projects):
                        continue  # Skip if already exists
                    
                    # Get Git remote info if it's a Git repo
                    git_remote = ""
                    if project.has_git:
                        git_info = self.project_discovery.get_git_remote_info(project.path)
                        git_remote = git_info.get('origin', '')
                    
                    # Add to database with all info
                    short_id = self.db.create_project(
                        name=project.name,
                        path=project.path,
                        description=f"Discovered via scan of {path}",
                        has_git=project.has_git,
                        git_remote_url=git_remote,
                        discovered_from='filesystem_scan'
                    )
                    
                    added_count += 1
                    
                except Exception as e:
                    self._add_system_message(f"Error adding {project.name}: {e}")
            
            if added_count > 0:
                self._add_system_message(f"✓ Added {added_count} new projects to database")
                self._add_system_message("Use 'projects' to see all tracked projects")
            else:
                self._add_system_message("No new projects added (may already exist in database)")
                
        except Exception as e:
            self._add_system_message(f"Error scanning path: {e}")
    
    def _suggest_paths(self) -> None:
        """Suggest project paths to scan"""
        try:
            self._add_system_message("Analyzing system for project path suggestions...")
            
            suggestions = self.project_discovery.suggest_project_paths()
            
            if not suggestions:
                self._add_system_message("No project path suggestions found")
                return
            
            claude_suggestions = [s for s in suggestions if s['source'] == 'claude_sessions']
            parent_suggestions = [s for s in suggestions if s['source'] == 'claude_parent']
            common_suggestions = [s for s in suggestions if s['source'] == 'common_directory']
            
            if claude_suggestions:
                self._add_system_message("Projects with Claude Code sessions:")
                for suggestion in claude_suggestions[:5]:
                    self._add_system_message(f"• {suggestion['name']} ({suggestion['session_count']} sessions)")
                    self._add_system_message(f"  {suggestion['path']}")
            
            if parent_suggestions:
                self._add_system_message("\nParent directories to scan:")
                for suggestion in parent_suggestions[:3]:
                    self._add_system_message(f"• {suggestion['path']}")
                    self._add_system_message(f"  Use: scan {suggestion['path']}")
            
            if common_suggestions:
                self._add_system_message("\nCommon project directories:")
                for suggestion in common_suggestions[:3]:
                    self._add_system_message(f"• {suggestion['path']}")
                    self._add_system_message(f"  Use: scan {suggestion['path']}")
            
        except Exception as e:
            self._add_system_message(f"Error getting suggestions: {e}")
    
    def _show_status(self) -> None:
        """Show system status"""
        try:
            if not self.db.is_initialized():
                status_text = """System Status:
• Cafedelic Server: Running
• Database: Not initialized
• Sessions: 0
• Projects: 0

Run 'init' to set up database."""
            else:
                projects = self.db.get_projects()
                sessions = self.db.get_sessions()
                running_sessions = self.session_manager.list_running_sessions()
                
                status_text = f"""System Status:
• Cafedelic Server: Running
• Database: Connected
• Projects: {len(projects)}
• Sessions: {len(sessions)}
• Running Sessions: {len(running_sessions)}
• Database Path: {self.db.db_path}"""
                
        except Exception as e:
            status_text = f"Error getting status: {e}"
            
        self._add_system_message(status_text)
    
    def _show_projects(self) -> None:
        """Show all projects"""
        try:
            if not self.db.is_initialized():
                self._add_system_message("Database not initialized. Run 'init' first.")
                return
                
            projects = self.db.get_projects()
            if not projects:
                self._add_system_message("No projects found.")
                return
            
            projects_text = "Projects:\n"
            for project in projects:
                activity_indicator = "●●●" if project.active_sessions > 0 else "○○○"
                last_activity = project.last_activity[:19] if project.last_activity else "Never"
                projects_text += f"• {project.short_id}: {project.name} [{project.status}] {activity_indicator}\n"
                projects_text += f"  Sessions: {project.session_count} | Active: {project.active_sessions} | Last: {last_activity}\n"
                projects_text += f"  Path: {project.path}\n"
            
            self._add_system_message(projects_text.strip())
            
        except Exception as e:
            self._add_system_message(f"Error getting projects: {e}")
    
    def _show_sessions(self) -> None:
        """Show active sessions"""
        try:
            if not self.db.is_initialized():
                self._add_system_message("Database not initialized. Run 'init' first.")
                return
                
            sessions = self.db.get_sessions()
            if not sessions:
                self._add_system_message("No sessions found.")
                return
            
            sessions_text = "Sessions:\n"
            for session in sessions:
                # Get tmux status
                tmux_info = self.session_manager.get_session_info(session.short_id)
                tmux_status = "🔴 Running" if tmux_info and tmux_info.is_running else "⚫ Not running"
                
                status_emoji = {
                    'active': '🟢',
                    'available': '⚪',
                    'detached': '🟡',
                    'stuck': '🔴'
                }.get(session.status, '❓')
                
                last_activity = session.last_activity[:19] if session.last_activity else "Never"
                sessions_text += f"• {session.short_id}: {session.name} {status_emoji} {session.status}\n"
                sessions_text += f"  Project: {session.project_short_id} ({session.project_name})\n"
                sessions_text += f"  Task: {session.task_description}\n"
                sessions_text += f"  Tmux: {tmux_status} | Last: {last_activity}\n"
                sessions_text += f"  Use 'open {session.short_id}' to start/resume\n"
            
            self._add_system_message(sessions_text.strip())
            
        except Exception as e:
            self._add_system_message(f"Error getting sessions: {e}")
    
    def _open_session(self, session_id: str) -> None:
        """Open/resume a Claude Code session"""
        try:
            if not self.db.is_initialized():
                self._add_system_message("Database not initialized. Run 'init' first.")
                return
            
            # Validate session exists
            session = self.db.get_session_by_id(session_id)
            if not session:
                self._add_system_message(f"Session '{session_id}' not found. Use 'sessions' to see available sessions.")
                return
            
            self._add_system_message(f"Opening session {session_id} ({session.name})...")
            
            # Check if session is already running
            tmux_info = self.session_manager.get_session_info(session_id)
            if tmux_info and tmux_info.is_running:
                self._add_system_message(f"Session is already running. Attaching to tmux session...")
                success = self.session_manager.attach_session(session_id)
            else:
                self._add_system_message(f"Starting new tmux session for {session.name}...")
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
            if not self.db.is_initialized():
                self._add_system_message("Database not initialized. Run 'init' first.")
                return
            
            # Validate session exists
            session = self.db.get_session_by_id(session_id)
            if not session:
                self._add_system_message(f"Session '{session_id}' not found.")
                return
            
            self._add_system_message(f"Killing session {session_id} ({session.name})...")
            
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
            if not self.db.is_initialized():
                self._add_system_message("Database not initialized. Run 'init' first.")
                return
            
            # Simple task delegation - find best available session
            sessions = self.db.get_sessions()
            available_sessions = [s for s in sessions if s.status == 'available']
            
            if not available_sessions:
                self._add_system_message("No available sessions found. All sessions are busy or stuck.")
                return
            
            # Pick first available session for now
            target_session = available_sessions[0]
            
            self._add_system_message(f"Delegating task: '{task}'")
            self._add_system_message(f"→ Assigned to session {target_session.short_id} ({target_session.name})")
            self._add_system_message(f"→ Use 'open {target_session.short_id}' to start working on this task")
            
            # Add activity log
            self.db.add_activity(
                session_id=target_session.short_id,
                activity_type='delegate',
                description=f"Task delegated: {task}"
            )
            
        except Exception as e:
            self._add_system_message(f"Error delegating task: {e}")
    
    def _load_sessions(self) -> None:
        """Load discovered Claude Code sessions into database"""
        try:
            if not self.db.is_initialized():
                self._add_system_message("Database not initialized. Run 'init' first.")
                return
            
            self._add_system_message("Discovering Claude Code sessions...")
            
            # Get summary of current state
            summary = self.session_loader.get_session_loading_summary()
            
            self._add_system_message(f"Found {summary['claude_sessions_discovered']} Claude sessions")
            self._add_system_message(f"Database has {summary['database_sessions_from_claude']} sessions from Claude")
            
            if summary['claude_sessions_not_in_db'] == 0:
                self._add_system_message("✓ All Claude sessions are already loaded in database")
                return
            
            self._add_system_message(f"Loading {summary['claude_sessions_not_in_db']} new sessions...")
            
            # Load the sessions
            results = self.session_loader.load_discovered_sessions()
            
            # Report results
            self._add_system_message(f"✓ Session loading complete:")
            self._add_system_message(f"  • Discovered: {results['discovered']} sessions")
            self._add_system_message(f"  • Loaded: {results['loaded']} new sessions")
            self._add_system_message(f"  • Skipped: {results['skipped']} (already existed)")
            
            if results['errors'] > 0:
                self._add_system_message(f"  • Errors: {results['errors']} sessions failed to load")
            
            if results['loaded'] > 0:
                self._add_system_message("Use 'sessions' to see all loaded sessions")
                
        except Exception as e:
            self._add_system_message(f"Error loading sessions: {e}")
    
    def _clear_chat(self) -> None:
        """Clear chat history"""
        self.messages = []
        self._render_messages()
        self._add_system_message("Chat cleared")


if __name__ == "__main__":
    app = SimpleChatApp()
    app.run()