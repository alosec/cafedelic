#!/usr/bin/env python3
"""
ActivityMonitor Widget
Bottom bar widget displaying real-time activity stream from all sessions.
"""

from textual.widgets import Static, Button, Label
from textual.containers import Horizontal, ScrollableContainer
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget
from textual.timer import Timer
from rich.text import Text
from datetime import datetime, timedelta
from typing import List, Dict, Any, Optional


class ActivityMonitor(Widget):
    """Real-time activity monitor for bottom bar"""
    
    DEFAULT_CSS = """
    ActivityMonitor {
        dock: bottom;
        height: 4;
        background: $panel;
        border: solid $primary;
    }
    
    ActivityMonitor .activity-header {
        dock: top;
        height: 1;
        background: $accent;
        padding: 0 1;
        text-align: center;
    }
    
    ActivityMonitor .activity-stream {
        height: 2;
        padding: 0 1;
        overflow-x: auto;
        overflow-y: hidden;
    }
    
    ActivityMonitor .activity-controls {
        dock: bottom;
        height: 1;
        background: $surface;
        padding: 0 1;
        layout: horizontal;
    }
    
    ActivityMonitor .control-btn {
        background: $accent;
        color: $text;
        margin: 0 1;
        min-width: 6;
        max-height: 1;
    }
    
    ActivityMonitor .control-btn:hover {
        background: $accent 80%;
    }
    
    ActivityMonitor .activity-item {
        margin: 0 2 0 0;
        text-wrap: nowrap;
    }
    
    ActivityMonitor .activity-item:hover {
        background: $primary 20%;
    }
    """

    class ActivityCleared(Message):
        """Message sent when activity is cleared"""
        def __init__(self) -> None:
            super().__init__()

    class ActivityFiltered(Message):
        """Message sent when activity filter changes"""
        def __init__(self, filter_type: str) -> None:
            self.filter_type = filter_type
            super().__init__()

    class ActivityExported(Message):
        """Message sent when activity export is requested"""
        def __init__(self) -> None:
            super().__init__()

    # Activity tracking
    activities: reactive[List[Dict[str, Any]]] = reactive([])
    auto_refresh: reactive[bool] = reactive(True)
    refresh_interval: reactive[int] = reactive(3)  # seconds
    filter_type: reactive[str] = reactive("all")  # all, files, commands, external
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._refresh_timer: Optional[Timer] = None

    def on_mount(self) -> None:
        """Initialize the activity monitor"""
        self._load_initial_activities()
        self._start_auto_refresh()

    def compose(self) -> ComposeResult:
        """Compose the activity monitor"""
        yield Static("═══ Real-time Activity Stream ═══", classes="activity-header")
        
        with ScrollableContainer(classes="activity-stream"):
            yield from self._build_activity_stream()
        
        with Horizontal(classes="activity-controls"):
            yield Static(f"Auto-refresh: {self.refresh_interval}s", id="refresh_status")
            yield Button("⏸️" if self.auto_refresh else "▶️", id="toggle_refresh", classes="control-btn")
            yield Button("🔄", id="manual_refresh", classes="control-btn")
            yield Button("🗑️", id="clear_activity", classes="control-btn")
            yield Button("📤", id="export_activity", classes="control-btn")
            
            # Filter buttons
            yield Static("Show:", id="filter_label")
            yield Button("All", id="filter_all", classes="control-btn")
            yield Button("Files", id="filter_files", classes="control-btn")
            yield Button("Cmds", id="filter_commands", classes="control-btn")

    def _build_activity_stream(self) -> ComposeResult:
        """Build the activity stream content"""
        filtered_activities = self._filter_activities()
        
        if not filtered_activities:
            no_activity_text = Text()
            no_activity_text.append("No recent activity", style="dim")
            yield Static(no_activity_text, classes="activity-item")
            return
        
        for activity in filtered_activities[-10:]:  # Show last 10 activities
            yield self._create_activity_item(activity)

    def _create_activity_item(self, activity: Dict[str, Any]) -> Widget:
        """Create a single activity item widget"""
        activity_text = Text()
        
        # Timestamp
        timestamp = activity.get('timestamp', datetime.now())
        if isinstance(timestamp, str):
            timestamp = datetime.fromisoformat(timestamp.replace('Z', '+00:00'))
        
        time_str = timestamp.strftime("[%H:%M]")
        activity_text.append(f"{time_str} ", style="dim")
        
        # Activity type indicator
        activity_type = activity.get('type', 'unknown')
        indicator = self._get_activity_indicator(activity_type)
        activity_text.append(f"{indicator} ", style=self._get_activity_color(activity_type))
        
        # Session name (if available)
        session_name = activity.get('session_name', activity.get('session_id', 'unknown'))
        if session_name and session_name != 'unknown':
            activity_text.append(f"{session_name[:12]}: ", style="bold")
        
        # Activity description
        description = activity.get('description', activity.get('message', 'No description'))
        activity_text.append(description[:80])  # Truncate long descriptions
        
        # Time ago
        time_ago = self._get_time_ago(timestamp)
        if time_ago:
            activity_text.append(f" ({time_ago})", style="dim")
        
        return Static(activity_text, classes="activity-item")

    def _get_activity_indicator(self, activity_type: str) -> str:
        """Get indicator symbol for activity type"""
        indicators = {
            'file_read': '👀',
            'file_write': '✏️',
            'file_create': '➕',
            'file_delete': '❌',
            'command': '⚡',
            'git': '🔀',
            'test': '🧪',
            'build': '🔨',
            'deploy': '🚀',
            'error': '❗',
            'session_start': '🎯',
            'session_end': '🏁',
            'chat': '💬',
            'analysis': '🔍'
        }
        return indicators.get(activity_type, '●')

    def _get_activity_color(self, activity_type: str) -> str:
        """Get color style for activity type"""
        colors = {
            'file_write': 'green',
            'file_create': 'cyan',
            'file_delete': 'red',
            'command': 'yellow',
            'error': 'red',
            'session_start': 'green',
            'session_end': 'blue',
            'analysis': 'magenta'
        }
        return colors.get(activity_type, 'white')

    def _get_time_ago(self, timestamp: datetime) -> str:
        """Get human-readable time ago string"""
        now = datetime.now(timestamp.tzinfo) if timestamp.tzinfo else datetime.now()
        delta = now - timestamp
        
        if delta.total_seconds() < 60:
            return f"{int(delta.total_seconds())}s ago"
        elif delta.total_seconds() < 3600:
            return f"{int(delta.total_seconds() / 60)}m ago"
        elif delta.total_seconds() < 86400:
            return f"{int(delta.total_seconds() / 3600)}h ago"
        else:
            return f"{delta.days}d ago"

    def _filter_activities(self) -> List[Dict[str, Any]]:
        """Filter activities based on current filter type"""
        if self.filter_type == "all":
            return self.activities
        elif self.filter_type == "files":
            return [a for a in self.activities if a.get('type', '').startswith('file_')]
        elif self.filter_type == "commands":
            return [a for a in self.activities if a.get('type') == 'command']
        elif self.filter_type == "external":
            return [a for a in self.activities if a.get('type') in ['git', 'build', 'deploy', 'test']]
        else:
            return self.activities

    def _load_initial_activities(self) -> None:
        """Load initial activity data (mocked for now)"""
        # Mock activities for demonstration
        now = datetime.now()
        mock_activities = [
            {
                'timestamp': now - timedelta(seconds=10),
                'type': 'file_write',
                'session_name': 'auth-feature',
                'description': 'Modified oauth.js - Added token refresh logic',
                'session_id': 'abc123'
            },
            {
                'timestamp': now - timedelta(minutes=2),
                'type': 'test',
                'session_name': 'auth-feature',
                'description': 'Ran test suite - 47 tests passed',
                'session_id': 'abc123'
            },
            {
                'timestamp': now - timedelta(minutes=4),
                'type': 'file_read',
                'session_name': 'ui-redesign',
                'description': 'Opened component library documentation',
                'session_id': 'def456'
            },
            {
                'timestamp': now - timedelta(minutes=6),
                'type': 'command',
                'session_name': 'test-suite',
                'description': 'Executed /analyze command',
                'session_id': 'ghi789'
            },
            {
                'timestamp': now - timedelta(minutes=8),
                'type': 'git',
                'session_name': 'auth-feature',
                'description': 'Committed changes - "OAuth token validation"',
                'session_id': 'abc123'
            }
        ]
        
        self.activities = mock_activities

    def _start_auto_refresh(self) -> None:
        """Start auto-refresh timer"""
        if self.auto_refresh and not self._refresh_timer:
            self._refresh_timer = self.set_interval(self.refresh_interval, self._refresh_activities)

    def _stop_auto_refresh(self) -> None:
        """Stop auto-refresh timer"""
        if self._refresh_timer:
            self._refresh_timer.stop()
            self._refresh_timer = None

    def _refresh_activities(self) -> None:
        """Refresh activity data"""
        # TODO: Implement real activity data fetching
        # For now, just simulate new activity
        if self.activities:
            # Add a mock new activity every refresh
            new_activity = {
                'timestamp': datetime.now(),
                'type': 'file_write',
                'session_name': 'live-session',
                'description': f'Auto-refresh update {len(self.activities)}',
                'session_id': 'live123'
            }
            
            # Add to activities and keep last 50
            updated_activities = list(self.activities)
            updated_activities.append(new_activity)
            self.activities = updated_activities[-50:]
            
            # Rebuild stream
            self._rebuild_stream()

    def _rebuild_stream(self) -> None:
        """Rebuild the activity stream"""
        stream_container = self.query_one(".activity-stream", ScrollableContainer)
        stream_container.remove_children()
        
        # Mount widgets from the generator
        for widget in self._build_activity_stream():
            stream_container.mount(widget)
        
        # Auto-scroll to end
        stream_container.scroll_end(animate=False)

    def add_activity(self, activity: Dict[str, Any]) -> None:
        """Add a new activity to the stream"""
        updated_activities = list(self.activities)
        updated_activities.append(activity)
        self.activities = updated_activities[-50:]  # Keep last 50
        self._rebuild_stream()

    def clear_activities(self) -> None:
        """Clear all activities"""
        self.activities = []
        self._rebuild_stream()
        self.post_message(self.ActivityCleared())

    def set_filter(self, filter_type: str) -> None:
        """Set activity filter"""
        self.filter_type = filter_type
        self._rebuild_stream()
        self.post_message(self.ActivityFiltered(filter_type))

    def toggle_auto_refresh(self) -> None:
        """Toggle auto-refresh on/off"""
        self.auto_refresh = not self.auto_refresh
        
        if self.auto_refresh:
            self._start_auto_refresh()
        else:
            self._stop_auto_refresh()
        
        # Update button text
        toggle_btn = self.query_one("#toggle_refresh", Button)
        toggle_btn.label = "⏸️" if self.auto_refresh else "▶️"

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle control button presses"""
        button_id = event.button.id
        
        if button_id == "toggle_refresh":
            self.toggle_auto_refresh()
        elif button_id == "manual_refresh":
            self._refresh_activities()
        elif button_id == "clear_activity":
            self.clear_activities()
        elif button_id == "export_activity":
            self.post_message(self.ActivityExported())
        elif button_id == "filter_all":
            self.set_filter("all")
        elif button_id == "filter_files":
            self.set_filter("files")
        elif button_id == "filter_commands":
            self.set_filter("commands")

    def on_unmount(self) -> None:
        """Clean up when widget is unmounted"""
        self._stop_auto_refresh()