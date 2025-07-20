#!/usr/bin/env python3
"""
SessionGrid Widget
Grid layout displaying session cards with status, progress, and quick actions.
"""

from textual.widgets import Static, Button, ProgressBar
from textual.containers import Vertical, Horizontal, ScrollableContainer, Grid
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget
from rich.text import Text
from datetime import datetime, timedelta
from typing import List, Dict, Any, Optional

from ..data.claude_code_data import get_sessions, get_session_by_id, get_activity_emoji


class SessionCard(Widget):
    """Individual session card widget"""
    
    DEFAULT_CSS = """
    SessionCard {
        width: 25;
        height: 10;
        background: $panel;
        border: solid $primary;
        margin: 1;
        padding: 1;
    }
    
    SessionCard:hover {
        border: solid $accent;
        background: $panel 120%;
    }
    
    SessionCard.selected {
        border: solid $accent 150%;
        background: $accent 20%;
    }
    
    SessionCard .card-header {
        dock: top;
        height: 2;
        text-align: center;
    }
    
    SessionCard .card-content {
        height: 1fr;
        layout: vertical;
    }
    
    SessionCard .card-actions {
        dock: bottom;
        height: 2;
        layout: horizontal;
    }
    
    SessionCard .action-btn {
        background: $accent;
        color: $text;
        margin: 0 1;
        min-width: 6;
        max-height: 1;
    }
    
    SessionCard .action-btn:hover {
        background: $accent 80%;
    }
    
    SessionCard .status-indicator {
        text-align: center;
        margin: 0 0 1 0;
    }
    
    SessionCard .progress-section {
        margin: 1 0;
    }
    """

    class CardClicked(Message):
        """Message sent when card is clicked"""
        def __init__(self, session_id: str) -> None:
            self.session_id = session_id
            super().__init__()

    class ActionRequested(Message):
        """Message sent when an action is requested"""
        def __init__(self, action: str, session_id: str) -> None:
            self.action = action
            self.session_id = session_id
            super().__init__()

    def __init__(self, session_data: Dict[str, Any], **kwargs):
        super().__init__(**kwargs)
        self.session_data = session_data
        self.session_id = session_data.get('id', '')

    def compose(self) -> ComposeResult:
        """Compose the session card"""
        session = self.session_data
        
        # Header with session name
        with Vertical(classes="card-header"):
            name_text = Text()
            name_text.append(session.get('name', session.get('id', 'Unknown'))[:20], style="bold")
            yield Static(name_text)
            
            project_text = Text()
            project_text.append(f"📁 {session.get('project', 'No Project')}", style="dim")
            yield Static(project_text)

        # Content
        with Vertical(classes="card-content"):
            # Status indicator
            status = session.get('status', 'unknown')
            status_emoji = self._get_status_emoji(status)
            duration = session.get('duration', '0m')
            
            status_text = Text()
            status_text.append(f"{status_emoji} {status.title()}", style=self._get_status_color(status))
            yield Static(status_text, classes="status-indicator")
            
            duration_text = Text()
            duration_text.append(f"⏱️ {duration}", style="dim")
            yield Static(duration_text)
            
            # Progress section
            with Vertical(classes="progress-section"):
                progress = session.get('progress', 0)
                progress_text = f"Progress: {int(progress * 100)}%"
                yield Static(progress_text, classes="dim-text")
                yield ProgressBar(total=100, progress=progress * 100, show_percentage=False)
            
            # Activity indicator
            activity_level = session.get('activity_level', 0)
            activity_text = Text()
            activity_text.append("Activity: ", style="dim")
            activity_text.append(get_activity_emoji(activity_level))
            yield Static(activity_text)

        # Actions
        with Horizontal(classes="card-actions"):
            yield Button("View", id=f"view_{self.session_id}", classes="action-btn")
            yield Button("Chat", id=f"chat_{self.session_id}", classes="action-btn")

    def _get_status_emoji(self, status: str) -> str:
        """Get emoji for session status"""
        status_map = {
            'planning': '🎯',
            'analyzing': '🔍',
            'implementing': '⚙️',
            'reviewing': '👀',
            'stuck': '⚠️',
            'idle': '💤',
            'available': '○'
        }
        return status_map.get(status, '❓')

    def _get_status_color(self, status: str) -> str:
        """Get color style for status"""
        color_map = {
            'planning': 'blue',
            'analyzing': 'yellow',
            'implementing': 'green',
            'reviewing': 'cyan',
            'stuck': 'red',
            'idle': 'dim',
            'available': 'white'
        }
        return color_map.get(status, 'white')

    def on_click(self) -> None:
        """Handle card click"""
        self.post_message(self.CardClicked(self.session_id))

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle action button presses"""
        button_id = event.button.id
        
        if button_id.startswith("view_"):
            self.post_message(self.ActionRequested("view", self.session_id))
        elif button_id.startswith("chat_"):
            self.post_message(self.ActionRequested("chat", self.session_id))

    def set_selected(self, selected: bool) -> None:
        """Set card selection state"""
        if selected:
            self.add_class("selected")
        else:
            self.remove_class("selected")

    def update_session_data(self, session_data: Dict[str, Any]) -> None:
        """Update session data and refresh display"""
        self.session_data = session_data
        # TODO: Refresh card content without full rebuild


class SessionGrid(Widget):
    """Grid layout for session cards"""
    
    DEFAULT_CSS = """
    SessionGrid {
        width: 1fr;
        height: 1fr;
        background: $surface;
    }
    
    SessionGrid .grid-header {
        dock: top;
        height: 3;
        background: $panel;
        padding: 1;
        layout: horizontal;
    }
    
    SessionGrid .grid-content {
        height: 1fr;
        padding: 1;
    }
    
    SessionGrid .grid-controls {
        margin: 0 1 0 0;
    }
    
    SessionGrid .control-btn {
        background: $accent;
        color: $text;
        margin: 0 1;
        min-width: 8;
    }
    
    SessionGrid .control-btn:hover {
        background: $accent 80%;
    }
    
    SessionGrid .filter-info {
        margin: 0;
        color: $text-muted;
        text-align: right;
    }
    
    SessionGrid .sessions-grid {
        layout: grid;
        grid-size: 4;
        grid-gutter: 1 1;
        height: 1fr;
    }
    
    SessionGrid .empty-state {
        text-align: center;
        padding: 4;
        color: $text-muted;
    }
    
    SessionGrid .bold-text {
        text-style: bold;
    }
    
    SessionGrid .dim-text {
        text-style: dim;
    }
    """

    class SessionSelected(Message):
        """Message sent when a session is selected"""
        def __init__(self, session_id: str) -> None:
            self.session_id = session_id
            super().__init__()

    class SessionActionRequested(Message):
        """Message sent when a session action is requested"""
        def __init__(self, action: str, session_id: str) -> None:
            self.action = action
            self.session_id = session_id
            super().__init__()

    class FilterChanged(Message):
        """Message sent when filter changes"""
        def __init__(self, filter_type: str) -> None:
            self.filter_type = filter_type
            super().__init__()

    # Grid state
    sessions: reactive[List[Dict[str, Any]]] = reactive([])
    filter_type: reactive[str] = reactive("all")  # all, active, stuck, recent
    sort_by: reactive[str] = reactive("activity")  # activity, duration, progress, created
    selected_session: reactive[str] = reactive("")

    def compose(self) -> ComposeResult:
        """Compose the session grid"""
        # Header with controls
        with Horizontal(classes="grid-header"):
            with Horizontal(classes="grid-controls"):
                yield Static("Filter:", classes="bold-text")
                yield Button("All", id="filter_all", classes="control-btn")
                yield Button("Active", id="filter_active", classes="control-btn")
                yield Button("Stuck", id="filter_stuck", classes="control-btn")
                yield Button("Recent", id="filter_recent", classes="control-btn")
                
                yield Static("Sort:", classes="bold-text")
                yield Button("Activity", id="sort_activity", classes="control-btn")
                yield Button("Duration", id="sort_duration", classes="control-btn")
                yield Button("Progress", id="sort_progress", classes="control-btn")
            
            with Vertical(classes="filter-info"):
                yield Static(self._get_filter_info_text(), id="filter_info")

        # Grid content
        with ScrollableContainer(classes="grid-content"):
            yield from self._build_sessions_grid()

    def on_mount(self) -> None:
        """Initialize the grid"""
        self._load_sessions()

    def _build_sessions_grid(self) -> ComposeResult:
        """Build the sessions grid"""
        filtered_sessions = self._filter_and_sort_sessions()
        
        if not filtered_sessions:
            yield from self._build_empty_state()
            return
        
        with Grid(classes="sessions-grid"):
            for session in filtered_sessions:
                yield SessionCard(session)

    def _build_empty_state(self) -> ComposeResult:
        """Build empty state when no sessions match filter"""
        with Vertical(classes="empty-state"):
            if self.filter_type == "all":
                empty_text = Text()
                empty_text.append("🎯 No Sessions Found\n\n", style="bold")
                empty_text.append("Create a new session to get started")
            else:
                empty_text = Text()
                empty_text.append(f"No {self.filter_type} sessions found\n\n", style="bold")
                empty_text.append("Try changing the filter or creating new sessions")
            
            yield Static(empty_text)

    def _filter_and_sort_sessions(self) -> List[Dict[str, Any]]:
        """Filter and sort sessions based on current settings"""
        filtered = list(self.sessions)
        
        # Apply filter
        if self.filter_type == "active":
            filtered = [s for s in filtered if s.get('status') not in ['idle', 'archived']]
        elif self.filter_type == "stuck":
            filtered = [s for s in filtered if s.get('status') == 'stuck']
        elif self.filter_type == "recent":
            # Filter to sessions modified in last 24 hours
            cutoff = datetime.now() - timedelta(hours=24)
            # TODO: Add proper timestamp filtering
            pass
        
        # Apply sorting
        if self.sort_by == "activity":
            filtered.sort(key=lambda s: s.get('activity_level', 0), reverse=True)
        elif self.sort_by == "duration":
            # TODO: Add proper duration sorting
            pass
        elif self.sort_by == "progress":
            filtered.sort(key=lambda s: s.get('progress', 0), reverse=True)
        elif self.sort_by == "created":
            # TODO: Add proper creation time sorting
            pass
        
        return filtered

    def _get_filter_info_text(self) -> str:
        """Get filter information text"""
        total = len(self.sessions)
        filtered = len(self._filter_and_sort_sessions())
        
        if self.filter_type == "all":
            return f"Showing all {total} sessions"
        else:
            return f"Showing {filtered} of {total} sessions ({self.filter_type})"

    def _load_sessions(self) -> None:
        """Load sessions data"""
        self.sessions = get_sessions()

    def _rebuild_grid(self) -> None:
        """Rebuild the entire grid"""
        content_container = self.query_one(".grid-content", ScrollableContainer)
        content_container.remove_children()
        # Mount widgets from the generator
        for widget in self._build_sessions_grid():
            content_container.mount(widget)
        
        # Update filter info
        filter_info = self.query_one("#filter_info", Static)
        filter_info.update(self._get_filter_info_text())

    def set_filter(self, filter_type: str) -> None:
        """Set session filter"""
        self.filter_type = filter_type
        self._rebuild_grid()
        self.post_message(self.FilterChanged(filter_type))

    def set_sort(self, sort_by: str) -> None:
        """Set session sorting"""
        self.sort_by = sort_by
        self._rebuild_grid()

    def refresh_sessions(self) -> None:
        """Refresh sessions data"""
        self._load_sessions()
        self._rebuild_grid()

    def select_session(self, session_id: str) -> None:
        """Select a specific session"""
        self.selected_session = session_id
        
        # Update card selection states
        for card in self.query("SessionCard"):
            if hasattr(card, 'session_id'):
                card.set_selected(card.session_id == session_id)

    def on_session_card_card_clicked(self, event: SessionCard.CardClicked) -> None:
        """Handle session card click"""
        self.select_session(event.session_id)
        self.post_message(self.SessionSelected(event.session_id))

    def on_session_card_action_requested(self, event: SessionCard.ActionRequested) -> None:
        """Handle session card action"""
        self.post_message(self.SessionActionRequested(event.action, event.session_id))

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle control button presses"""
        button_id = event.button.id
        
        if button_id.startswith("filter_"):
            filter_type = button_id.replace("filter_", "")
            self.set_filter(filter_type)
        elif button_id.startswith("sort_"):
            sort_type = button_id.replace("sort_", "")
            self.set_sort(sort_type)

    def add_session(self, session_data: Dict[str, Any]) -> None:
        """Add a new session to the grid"""
        updated_sessions = list(self.sessions)
        updated_sessions.append(session_data)
        self.sessions = updated_sessions
        self._rebuild_grid()

    def remove_session(self, session_id: str) -> None:
        """Remove a session from the grid"""
        updated_sessions = [s for s in self.sessions if s.get('id') != session_id]
        self.sessions = updated_sessions
        self._rebuild_grid()

    def update_session(self, session_id: str, session_data: Dict[str, Any]) -> None:
        """Update a session in the grid"""
        updated_sessions = []
        for session in self.sessions:
            if session.get('id') == session_id:
                updated_sessions.append(session_data)
            else:
                updated_sessions.append(session)
        
        self.sessions = updated_sessions
        self._rebuild_grid()

    def get_session_count(self) -> Dict[str, int]:
        """Get session counts by status"""
        all_sessions = self.sessions
        return {
            'total': len(all_sessions),
            'active': len([s for s in all_sessions if s.get('status') not in ['idle', 'archived']]),
            'stuck': len([s for s in all_sessions if s.get('status') == 'stuck']),
            'idle': len([s for s in all_sessions if s.get('status') == 'idle'])
        }