#!/usr/bin/env python3
"""
Session Tabs Component
Task-state aware session management with tabbed interface.
"""

from textual.app import ComposeResult
from textual.containers import Horizontal, Vertical
from textual.widgets import TabbedContent, TabPane, Static, Button, ProgressBar
from textual.widget import Widget
from textual.reactive import reactive
from textual.message import Message

from ..data.mock_data import get_sessions, get_session_status_emoji, MockSession


class SessionDetailPane(Widget):
    """Individual session detail pane"""
    
    DEFAULT_CSS = """
    SessionDetailPane {
        padding: 1;
    }
    
    SessionDetailPane .session-header {
        background: $panel;
        padding: 1;
        margin: 0 0 1 0;
        border: round $primary;
    }
    
    SessionDetailPane .session-status {
        text-style: bold;
        margin: 0 0 1 0;
    }
    
    SessionDetailPane .files-list {
        background: $surface;
        padding: 1;
        margin: 1 0;
        height: 6;
    }
    
    SessionDetailPane .controls {
        dock: bottom;
        height: 3;
        background: $panel;
        padding: 1;
    }
    
    SessionDetailPane Button {
        margin: 0 1 0 0;
    }
    """
    
    class SendCommand(Message):
        """Message for sending command to session"""
        def __init__(self, session_id: str, command: str) -> None:
            self.session_id = session_id
            self.command = command
            super().__init__()
    
    def __init__(self, session: MockSession) -> None:
        super().__init__()
        self.session = session
    
    def compose(self) -> ComposeResult:
        """Compose session detail pane"""
        emoji = get_session_status_emoji(self.session.status)
        
        with Vertical():
            # Session header
            with Vertical(classes="session-header"):
                yield Static(f"{emoji} {self.session.name} [{self.session.id}]", classes="session-status")
                yield Static(f"Task: {self.session.task}")
                yield Static(f"Status: {self.session.status.title()}... {self.session.task.split()[0].lower()} patterns")
                progress_bar = ProgressBar(total=100)
                progress_bar.progress = int(self.session.progress * 100)
                yield progress_bar
            
            # Files in context
            with Vertical(classes="files-list"):
                yield Static("Files in Context:", classes="label")
                for file_path in self.session.files_context[:3]:  # Show top 3 files
                    yield Static(f"📄 {file_path}")
                if len(self.session.files_context) > 3:
                    yield Static(f"... and {len(self.session.files_context) - 3} more")
            
            # Recent activity
            yield Static(f"Last Activity: {self.session.duration} ago - Working on {self.session.task.split()[0].lower()}")
            
            # Control buttons
            with Horizontal(classes="controls"):
                if self.session.status == "planning":
                    yield Button("/act", variant="primary", id="cmd_act")
                    yield Button("/keep-planning", id="cmd_keep_planning") 
                elif self.session.status == "analyzing":
                    yield Button("/act", variant="primary", id="cmd_act")
                    yield Button("/keep-analyzing", id="cmd_keep_analyzing")
                elif self.session.status == "stuck":
                    yield Button("/coordinate", variant="warning", id="cmd_coordinate")
                    yield Button("/help", id="cmd_help")
                else:
                    yield Button("/plan", id="cmd_plan")
                    yield Button("/analyze", id="cmd_analyze")
                
                yield Button("Send Message", id="send_message")
                yield Button("Handoff Task", id="handoff")
    
    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle button presses"""
        button_id = event.button.id
        
        if button_id.startswith("cmd_"):
            command = button_id.replace("cmd_", "/")
            self.post_message(self.SendCommand(self.session.id, command))
        elif button_id == "send_message":
            # In real implementation, open message dialog
            pass
        elif button_id == "handoff":
            # In real implementation, open handoff dialog
            pass


class SessionTabsWidget(Widget):
    """Widget managing tabbed session interface"""
    
    DEFAULT_CSS = """
    SessionTabsWidget {
        height: 1fr;
    }
    
    SessionTabsWidget TabbedContent {
        height: 1fr;
    }
    
    .tab-label {
        padding: 0 1;
    }
    """
    
    class CommandSent(Message):
        """Message when command is sent to session"""
        def __init__(self, session_id: str, command: str) -> None:
            self.session_id = session_id
            self.command = command
            super().__init__()
    
    def compose(self) -> ComposeResult:
        """Compose the session tabs"""
        sessions = get_sessions()
        active_sessions = [s for s in sessions if s.status != 'available']
        
        with TabbedContent():
            for session in active_sessions:
                emoji = get_session_status_emoji(session.status)
                tab_title = f"{session.name}: {session.status.title()} {emoji}"
                
                with TabPane(tab_title, id=f"tab_{session.id}"):
                    yield SessionDetailPane(session)
            
            # Add new session tab
            with TabPane("+ New", id="tab_new"):
                yield Static("Create New Session", classes="title")
                yield Static("Select a project and task to start a new Claude Code session")
                yield Button("Create Session", variant="primary", id="create_session")
    
    def on_session_detail_pane_send_command(self, message: SessionDetailPane.SendCommand) -> None:
        """Handle command sent from session detail pane"""
        self.post_message(self.CommandSent(message.session_id, message.command))
    
    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle button presses"""
        if event.button.id == "create_session":
            # In real implementation, open session creation dialog
            pass