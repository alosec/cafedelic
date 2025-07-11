#!/usr/bin/env python3
"""
Task Delegation Feed Component
Displays real-time task delegation status and coordination opportunities.
"""

from textual.app import ComposeResult
from textual.containers import Vertical, Horizontal, VerticalScroll
from textual.widgets import Static, Button
from textual.widget import Widget
from textual.reactive import reactive
from datetime import datetime, timedelta

from ..data.mock_data import get_tasks, get_task_status_emoji, get_sessions, MockTask


class TaskFeedItem(Widget):
    """Individual task feed item"""
    
    DEFAULT_CSS = """
    TaskFeedItem {
        height: auto;
        margin: 0 0 1 0;
        padding: 1;
        background: $surface;
        border-left: thick $primary;
    }
    
    TaskFeedItem.completed {
        border-left: thick $success;
        background: $success 20%;
    }
    
    TaskFeedItem.failed {
        border-left: thick $error;
        background: $error 20%;
    }
    
    TaskFeedItem.in_progress {
        border-left: thick $warning;
        background: $warning 20%;
    }
    
    TaskFeedItem .timestamp {
        color: $text-muted;
        text-style: italic;
    }
    
    TaskFeedItem .task-description {
        text-style: bold;
        margin: 0 0 1 0;
    }
    
    TaskFeedItem .session-info {
        color: $text-muted;
    }
    """
    
    def __init__(self, task: MockTask, session_name: str = "") -> None:
        super().__init__()
        self.task_data = task  # Renamed to avoid conflict with Widget.task
        self.session_name = session_name
        self.add_class(task.status)
    
    def compose(self) -> ComposeResult:
        """Compose task feed item"""
        emoji = get_task_status_emoji(self.task_data.status)
        time_ago = self._format_time_ago(self.task_data.created_at)
        
        with Vertical():
            with Horizontal():
                yield Static(f"[{time_ago}]", classes="timestamp")
                yield Static(f"{emoji}", classes="status-icon")
                
            yield Static(f"{self.task_data.description}", classes="task-description")
            
            if self.session_name:
                yield Static(f"Session: {self.session_name}", classes="session-info")
            elif self.task_data.status == "pending":
                yield Static("⏳ Awaiting session assignment", classes="session-info")
            
            # Show priority for pending tasks
            if self.task_data.status == "pending" and self.task_data.priority == "high":
                yield Static("🔥 High Priority", classes="priority-high")
    
    def _format_time_ago(self, timestamp: datetime) -> str:
        """Format timestamp as time ago"""
        now = datetime.now()
        diff = now - timestamp
        
        if diff.total_seconds() < 60:
            return "just now"
        elif diff.total_seconds() < 3600:
            minutes = int(diff.total_seconds() / 60)
            return f"{minutes}m ago"
        else:
            hours = int(diff.total_seconds() / 3600)
            return f"{hours}h ago"


class TaskFeedWidget(Widget):
    """Widget displaying task delegation feed"""
    
    DEFAULT_CSS = """
    TaskFeedWidget {
        height: 1fr;
        border: solid $primary;
        margin: 1 0;
    }
    
    TaskFeedWidget .header {
        dock: top;
        height: 3;
        background: $panel;
        padding: 1;
    }
    
    TaskFeedWidget .feed-controls {
        dock: top;
        height: 1;
        background: $surface;
        padding: 0 1;
    }
    
    TaskFeedWidget VerticalScroll {
        height: 1fr;
    }
    
    TaskFeedWidget Button {
        margin: 0 1 0 0;
    }
    """
    
    # Reactive properties for demo
    auto_refresh: reactive[bool] = reactive(True)
    
    def compose(self) -> ComposeResult:
        """Compose the task feed widget"""
        with Vertical():
            # Header
            with Vertical(classes="header"):
                yield Static("🎯 Task Delegation Feed", classes="title")
                yield Static("Real-time task status and coordination opportunities")
            
            # Feed controls
            with Horizontal(classes="feed-controls"):
                yield Button("⟳ Refresh", id="refresh")
                yield Button("⚙️ Filter", id="filter")
                yield Button("🔍 Search", id="search")
                yield Static("Auto: ●", classes="auto-indicator", id="auto_indicator")
            
            # Feed content
            with VerticalScroll():
                yield self._build_feed_content()
    
    def _build_feed_content(self) -> Vertical:
        """Build the main feed content"""
        sessions = {s.id: s.name for s in get_sessions()}
        tasks = sorted(get_tasks(), key=lambda t: t.created_at, reverse=True)
        
        feed_content = Vertical()
        
        # Add some synthetic coordination messages
        feed_content.compose_add_child(self._create_coordination_message(
            "🔄 auth-refactor: Ready for /act command - plan complete",
            "2m ago"
        ))
        
        # Add actual tasks
        for task in tasks:
            session_name = sessions.get(task.session_id, "Unassigned")
            feed_content.compose_add_child(TaskFeedItem(task, session_name))
        
        # Add more synthetic messages for demo
        feed_content.compose_add_child(self._create_coordination_message(
            "🎯 Task suggested: Handoff auth tokens → database-opt session",
            "5m ago"
        ))
        
        feed_content.compose_add_child(self._create_coordination_message(
            "⚠️ database-opt: Stuck - requires coordination with auth session",
            "10m ago"
        ))
        
        return feed_content
    
    def _create_coordination_message(self, message: str, time_ago: str) -> Static:
        """Create a coordination/system message"""
        return Static(f"[{time_ago}] {message}", classes="coordination-message")
    
    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle button presses"""
        if event.button.id == "refresh":
            self._refresh_feed()
        elif event.button.id == "filter":
            # In real implementation, show filter dialog
            pass
        elif event.button.id == "search":
            # In real implementation, show search dialog  
            pass
    
    def _refresh_feed(self) -> None:
        """Refresh the feed content"""
        # In real implementation, this would fetch fresh data
        scroll_view = self.query_one(VerticalScroll)
        scroll_view.remove_children()
        scroll_view.mount(self._build_feed_content())
    
    def on_mount(self) -> None:
        """Setup auto-refresh timer"""
        if self.auto_refresh:
            self.set_interval(10.0, self._auto_refresh)  # Refresh every 10 seconds
    
    def _auto_refresh(self) -> None:
        """Auto refresh the feed"""
        if self.auto_refresh:
            self._refresh_feed()