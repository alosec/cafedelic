#!/usr/bin/env python3
"""
SessionViewPane Widget
Individual session management view with workflow commands and activity feed.
"""

from textual.widgets import Static, Button, ProgressBar
from textual.containers import ScrollableContainer
from textual.containers import Vertical, Horizontal
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget
from rich.text import Text
from datetime import datetime
from typing import Optional, Dict, Any

from ..data.claude_code_data import (
    get_session_by_id, get_context_files_by_session, 
    get_activity_emoji
)


class SessionViewPane(Widget):
    """Session view pane for individual session management"""
    
    DEFAULT_CSS = """
    SessionViewPane {
        width: 1fr;
        height: 1fr;
        layout: vertical;
    }
    
    SessionViewPane .session-header {
        dock: top;
        height: 3;
        background: $panel;
        padding: 1;
        border: solid $primary;
    }
    
    SessionViewPane .workflow-commands {
        height: 3;
        background: $surface;
        padding: 1;
        border: solid $accent;
    }
    
    SessionViewPane .content-area {
        height: 1fr;
        layout: horizontal;
    }
    
    SessionViewPane .context-files {
        width: 1fr;
        border: solid $primary;
        margin: 0 1 0 0;
    }
    
    SessionViewPane .activity-feed {
        width: 1fr;
        border: solid $primary;
    }
    
    SessionViewPane .workflow-btn {
        background: $accent;
        color: $text;
        margin: 0 1;
        padding: 0 1;
        border: none;
        min-width: 0;
    }
    
    SessionViewPane .workflow-btn:hover {
        background: $accent 80%;
    }
    
    SessionViewPane .health-bar {
        dock: bottom;
        height: 2;
        background: $panel;
        padding: 1;
    }
    """

    class CommandSent(Message):
        """Message sent when a workflow command is executed"""
        def __init__(self, session_id: str, command: str) -> None:
            self.session_id = session_id
            self.command = command
            super().__init__()

    session_id: reactive[str] = reactive("")
    session_data: reactive[Optional[Dict[str, Any]]] = reactive(None)

    def __init__(self, session_id: str, **kwargs):
        super().__init__(**kwargs)
        self.session_id = session_id
        self.session_data = get_session_by_id(session_id)

    def compose(self) -> ComposeResult:
        """Compose the session view pane"""
        if not self.session_data:
            yield Static("❌ Session not found", id="error_message")
            return

        session = self.session_data

        # Session header
        with Vertical(classes="session-header"):
            header_text = Text()
            header_text.append(f"Session: {session.name} [{session.id[:8]}]", style="bold")
            header_text.append(f"    ● {session.status.title()}", style="green" if session.status != "stuck" else "red")
            header_text.append(f"    Duration: {session.duration}")
            yield Static(header_text)
            
            task_text = Text()
            task_text.append(f"Task: {session.task}")
            yield Static(task_text)

        # Workflow commands
        with Horizontal(classes="workflow-commands"):
            yield Static("Workflow Commands:", style="bold")
            yield Button("/plan", id="cmd_plan", classes="workflow-btn")
            yield Button("/analyze", id="cmd_analyze", classes="workflow-btn")
            yield Button("/act", id="cmd_act", classes="workflow-btn")
            yield Button("/review", id="cmd_review", classes="workflow-btn")
            yield Button("/coordinate", id="cmd_coordinate", classes="workflow-btn")

        # Content area
        with Horizontal(classes="content-area"):
            # Context files
            with Vertical(classes="context-files"):
                yield Static("📁 Context Files (Active Session)", style="bold")
                yield self._build_context_files()

            # Activity feed
            with Vertical(classes="activity-feed"):
                yield Static("📊 Real-Time Activity Feed", style="bold")
                yield self._build_activity_feed()

        # Health and progress bar
        with Vertical(classes="health-bar"):
            progress_text = f"Health: ● {session.status.title()}    Progress: {int(session.progress * 100)}%"
            yield Static(progress_text)
            yield ProgressBar(total=100, progress=session.progress * 100, show_percentage=False)

    def _build_context_files(self) -> Widget:
        """Build the context files section"""
        container = ScrollableContainer()
        
        if not self.session_data:
            return container

        context_files = get_context_files_by_session(self.session_id)
        
        if not context_files:
            container.mount(Static("No files in context"))
            return container

        for file in context_files:
            activity = get_activity_emoji(file.activity_level)
            status_text = self._get_file_status_text(file.status)
            
            file_text = Text()
            file_text.append("📄 ")
            file_text.append(f"{file.path} ", style="bold")
            file_text.append(f"{activity} ")
            file_text.append(f"[{status_text}]", style="dim")
            
            file_widget = Static(file_text)
            container.mount(file_widget)
        
        # Action buttons
        actions_container = Horizontal()
        actions_container.mount(Button("[Delegate File]", classes="workflow-btn"))
        actions_container.mount(Button("[Open in Editor]", classes="workflow-btn"))
        actions_container.mount(Button("[Run Tests]", classes="workflow-btn"))
        actions_container.mount(Button("[Add to Context]", classes="workflow-btn"))
        container.mount(actions_container)
        
        return container

    def _build_activity_feed(self) -> Widget:
        """Build the activity feed section"""
        container = ScrollableContainer()
        
        if not self.session_data:
            return container

        # Mock recent activity
        activities = [
            (datetime.now().strftime("[%H:%M]"), "● Modified oauth.js - Added token refresh logic (3s ago)"),
            (datetime.now().strftime("[%H:%M]"), "● Created test suite - Comprehensive OAuth tests (2m ago)"),
            (datetime.now().strftime("[%H:%M]"), "○ Read OAuth RFC - Section 3.2 token lifecycle (4m ago)"),
            (datetime.now().strftime("[%H:%M]"), "● Enhanced middleware - Better error handling (6m ago)"),
            (datetime.now().strftime("[%H:%M]"), "✓ Committed changes - \"OAuth token validation\" (8m ago)"),
        ]
        
        for timestamp, activity in activities:
            activity_text = Text()
            activity_text.append(f"{timestamp} ")
            activity_text.append(activity)
            
            activity_widget = Static(activity_text)
            container.mount(activity_widget)
        
        # Auto-refresh controls
        refresh_text = Text()
        refresh_text.append("Auto-refresh: 3s | Show: ")
        refresh_text.append("[All] [File Changes] [Commands] [External]", style="link")
        
        container.mount(Static(""))
        container.mount(Static(refresh_text))
        
        return container

    def _get_file_status_text(self, status: str) -> str:
        """Get human-readable file status text"""
        status_map = {
            'modified': 'Modified 5m ago',
            'created': 'Created 2h ago',
            'read': 'Read 15m ago',
            'referenced': 'Referenced'
        }
        return status_map.get(status, status)

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle workflow command button presses"""
        button_id = event.button.id
        
        if button_id and button_id.startswith("cmd_"):
            command = button_id.replace("cmd_", "/")
            self.post_message(self.CommandSent(self.session_id, command))

    def refresh_session_data(self) -> None:
        """Refresh session data and update display"""
        self.session_data = get_session_by_id(self.session_id)
        
        # In a real implementation, this would trigger a re-render
        # For now, we'll just update the reactive property
        if self.session_data:
            # Update progress bar
            progress_bar = self.query_one(ProgressBar)
            progress_bar.progress = self.session_data.progress * 100