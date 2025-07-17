#!/usr/bin/env python3
"""
Quick Chat Delegation Widget
Primary interface for rapid task assignment to Claude Code sessions.
"""

from textual.app import ComposeResult
from textual.containers import Horizontal, Vertical
from textual.widgets import Input, Button, Select, Static
from textual.widget import Widget
from textual.reactive import reactive
from textual.message import Message

from ..data.claude_code_data import get_sessions


class QuickChatWidget(Widget):
    """Widget for quick task delegation to Claude Code sessions"""
    
    DEFAULT_CSS = """
    QuickChatWidget {
        height: 6;
        border: solid $primary;
        margin: 1 0;
        padding: 1;
    }
    
    QuickChatWidget > Vertical > Horizontal {
        height: 1;
        margin: 0 0 1 0;
    }
    
    QuickChatWidget Input {
        width: 3fr;
    }
    
    QuickChatWidget Select {
        width: 1fr;
        margin: 0 1;
    }
    
    QuickChatWidget Button {
        width: auto;
        margin: 0 0 0 1;
    }
    
    .suggestions {
        height: 2;
        color: $text-muted;
        text-style: italic;
    }
    """
    
    class TaskDelegated(Message):
        """Message sent when a task is delegated"""
        def __init__(self, task: str, session_id: str, command: str = None) -> None:
            self.task = task
            self.session_id = session_id
            self.command = command
            super().__init__()
    
    task_input: reactive[str] = reactive("")
    selected_session: reactive[str] = reactive("")
    selected_command: reactive[str] = reactive("")
    
    def compose(self) -> ComposeResult:
        """Compose the quick chat widget"""
        with Vertical():
            yield Static("Quick Delegate", classes="label")
            
            with Horizontal():
                yield Input(
                    placeholder="Type task description...",
                    id="task_input"
                )
                
                # Session selector
                session_options = [(f"{s.name} ({s.status})", s.id) for s in get_sessions()]
                session_options.append(("+ New Session", "new"))
                yield Select(
                    session_options,
                    prompt="Session",
                    id="session_select"
                )
                
                # Command selector  
                command_options = [(cmd.name, cmd.name) for cmd in get_commands()]
                command_options.insert(0, ("Custom", ""))
                yield Select(
                    command_options,
                    prompt="Command",
                    id="command_select"
                )
                
                yield Button("Find Similar", id="find_similar")
                yield Button("Delegate", variant="primary", id="delegate")
            
            yield Static("", classes="suggestions", id="suggestions")
    
    def on_mount(self) -> None:
        """Handle widget mount"""
        self.query_one("#task_input", Input).focus()
    
    def on_input_changed(self, event: Input.Changed) -> None:
        """Handle task input changes"""
        if event.input.id == "task_input":
            self.task_input = event.value
            self._update_suggestions()
    
    def on_select_changed(self, event: Select.Changed) -> None:
        """Handle select dropdown changes"""
        if event.select.id == "session_select":
            self.selected_session = str(event.value) if event.value else ""
        elif event.select.id == "command_select":
            self.selected_command = str(event.value) if event.value else ""
    
    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle button presses"""
        if event.button.id == "find_similar":
            self._find_similar_sessions()
        elif event.button.id == "delegate":
            self._delegate_task()
    
    def _update_suggestions(self) -> None:
        """Update suggestion text based on current input"""
        suggestions_widget = self.query_one("#suggestions", Static)
        
        if not self.task_input:
            suggestions_widget.update("")
            return
            
        # Simple keyword matching for demo
        suggestions = []
        task_lower = self.task_input.lower()
        
        if "auth" in task_lower:
            suggestions.append("💡 auth-refactor session has OAuth context")
        if "test" in task_lower:
            suggestions.append("💡 Try /auth-tests command")
        if "ui" in task_lower or "theme" in task_lower:
            suggestions.append("💡 ui-components session is analyzing themes")
        if "database" in task_lower or "query" in task_lower:
            suggestions.append("💡 database-opt session needs coordination")
            
        suggestion_text = " • ".join(suggestions[:2])  # Limit to 2 suggestions
        suggestions_widget.update(suggestion_text)
    
    def _find_similar_sessions(self) -> None:
        """Find sessions with similar context (mock implementation)"""
        if not self.task_input:
            return
            
        suggestions_widget = self.query_one("#suggestions", Static)
        suggestions_widget.update(f"🔍 Finding sessions similar to: '{self.task_input}'...")
        
        # In real implementation, this would call find_relevant_chats()
        self.call_after_refresh(self._show_similar_results)
    
    def _show_similar_results(self) -> None:
        """Show similar session results (mock)"""
        suggestions_widget = self.query_one("#suggestions", Static)
        suggestions_widget.update("📋 Found: auth-refactor (85% match), security-audit (72% match)")
    
    def _delegate_task(self) -> None:
        """Delegate the current task"""
        if not self.task_input:
            return
            
        if not self.selected_session:
            suggestions_widget = self.query_one("#suggestions", Static) 
            suggestions_widget.update("⚠️ Please select a target session")
            return
        
        # Post delegation message
        self.post_message(self.TaskDelegated(
            task=self.task_input,
            session_id=self.selected_session,
            command=self.selected_command if self.selected_command else None
        ))
        
        # Clear input
        self.query_one("#task_input", Input).value = ""
        self.task_input = ""
        
        # Show confirmation
        suggestions_widget = self.query_one("#suggestions", Static)
        session_name = next((s.name for s in get_sessions() if s.id == self.selected_session), self.selected_session)
        suggestions_widget.update(f"✅ Task delegated to {session_name}")