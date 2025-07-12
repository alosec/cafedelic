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
        # Load initial messages
        self.messages = get_chat_messages()
        self._render_messages()
        
        # Focus the input
        self.query_one("#chat_input", Input).focus()
        
        # Show welcome message
        self._add_system_message("Cafedelic Chat Interface - Type 'help' for commands")
    
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
        elif message_lower == "sessions":
            self._show_sessions()
        elif message_lower == "clear":
            self._clear_chat()
        elif message_lower.startswith("delegate "):
            task = message[9:]  # Remove "delegate " prefix
            self._delegate_task(task)
        else:
            # Echo response for now
            self._add_system_message(f"Processing: {message}")
    
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
• sessions - List active Claude Code sessions
• delegate <task> - Delegate task to AI
• clear - Clear chat history"""
        self._add_system_message(help_text)
    
    def _show_status(self) -> None:
        """Show system status"""
        status_text = """System Status:
• Cafedelic Server: Running
• Active Sessions: 3
• Database: Connected
• Last Activity: 2 minutes ago"""
        self._add_system_message(status_text)
    
    def _show_sessions(self) -> None:
        """Show active sessions"""
        sessions_text = """Active Sessions:
• auth-refactor [Planning] - OAuth integration analysis
• ui-components [Implementing] - Dark theme system
• database-opt [Stuck] - Query performance issues"""
        self._add_system_message(sessions_text)
    
    def _delegate_task(self, task: str) -> None:
        """Delegate task to AI system"""
        if not task:
            self._add_system_message("Please specify a task to delegate")
            return
        
        self._add_system_message(f"Delegating task: '{task}'")
        self._add_system_message("→ Finding best session...")
        self._add_system_message("→ Task assigned to 'ui-components' session")
        self._add_system_message("✓ Delegation complete. Session status: Planning...")
    
    def _clear_chat(self) -> None:
        """Clear chat history"""
        self.messages = []
        self._render_messages()
        self._add_system_message("Chat cleared")


if __name__ == "__main__":
    app = SimpleChatApp()
    app.run()