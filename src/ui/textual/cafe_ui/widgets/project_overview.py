#!/usr/bin/env python3
"""
ProjectOverview Widget
Main content area showing project statistics, active sessions, and project actions.
"""

from textual.widgets import Static, Button, ProgressBar, DataTable
from textual.containers import Vertical, Horizontal, ScrollableContainer
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget
from rich.text import Text
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List

from ..data.claude_code_data import (
    get_projects, get_sessions, get_files_by_project,
    get_session_by_id, get_activity_emoji
)


class ProjectOverview(Widget):
    """Project overview widget showing stats and sessions"""
    
    DEFAULT_CSS = """
    ProjectOverview {
        width: 1fr;
        height: 1fr;
        background: $surface;
        padding: 1;
    }
    
    ProjectOverview .project-header {
        dock: top;
        height: 4;
        background: $panel;
        border: solid $primary;
        padding: 1;
        margin: 0 0 1 0;
    }
    
    ProjectOverview .project-stats {
        layout: horizontal;
        height: 3;
        margin: 0 0 1 0;
    }
    
    ProjectOverview .stat-card {
        width: 1fr;
        height: 3;
        background: $primary;
        margin: 0 1 0 0;
        padding: 1;
        border: solid $accent;
        text-align: center;
    }
    
    ProjectOverview .sessions-section {
        height: 1fr;
    }
    
    ProjectOverview .section-header {
        dock: top;
        height: 1;
        background: $accent;
        padding: 0 1;
        text-align: center;
    }
    
    ProjectOverview .sessions-table {
        height: 1fr;
        margin: 1 0;
    }
    
    ProjectOverview .quick-actions {
        dock: bottom;
        height: 3;
        background: $panel;
        padding: 1;
        layout: horizontal;
    }
    
    ProjectOverview .action-btn {
        background: $accent;
        color: $text;
        margin: 0 1;
        min-width: 12;
    }
    
    ProjectOverview .action-btn:hover {
        background: $accent 80%;
    }
    
    ProjectOverview .empty-state {
        text-align: center;
        padding: 4;
        color: $text-muted;
    }
    """

    class SessionSelected(Message):
        """Message sent when a session is selected"""
        def __init__(self, session_id: str) -> None:
            self.session_id = session_id
            super().__init__()

    class ChatRequested(Message):
        """Message sent when chat is requested for a session"""
        def __init__(self, session_id: str) -> None:
            self.session_id = session_id
            super().__init__()

    class ActionRequested(Message):
        """Message sent when an action is requested"""
        def __init__(self, action: str, project_name: str) -> None:
            self.action = action
            self.project_name = project_name
            super().__init__()

    # Current project
    project_name: reactive[str] = reactive("")
    project_data: reactive[Optional[Dict[str, Any]]] = reactive(None)

    def compose(self) -> ComposeResult:
        """Compose the project overview"""
        if not self.project_name:
            yield from self._build_empty_state()
            return

        # Project header
        with Vertical(classes="project-header"):
            yield from self._build_project_header()

        # Statistics cards
        with Horizontal(classes="project-stats"):
            yield self._build_stats_cards()

        # Sessions section
        with Vertical(classes="sessions-section"):
            yield Static("🎯 Active Sessions", classes="section-header")
            yield self._build_sessions_table()

        # Quick actions
        with Horizontal(classes="quick-actions"):
            yield self._build_quick_actions()

    def _build_empty_state(self) -> ComposeResult:
        """Build empty state when no project is selected"""
        with Vertical(classes="empty-state"):
            empty_text = Text()
            empty_text.append("📁 No Project Selected\n\n", style="bold")
            empty_text.append("Select a project from the file tree\n")
            empty_text.append("to view project overview and manage sessions")
            
            yield Static(empty_text)

    def _build_project_header(self) -> ComposeResult:
        """Build project header with name and status"""
        with Vertical():
            # Project title
            title_text = Text()
            title_text.append(f"📁 {self.project_name}", style="bold")
            
            if self.project_data:
                status = self.project_data.get('status', 'unknown')
                status_emoji = self._get_project_status_emoji(status)
                title_text.append(f"  {status_emoji} {status.title()}")
            
            yield Static(title_text)
            
            # Project path and description
            if self.project_data:
                path_text = f"📂 {self.project_data.get('path', 'Unknown path')}"
                yield Static(path_text, classes="dim-text")
                
                description = self.project_data.get('description', 'No description')
                if description and description != 'No description':
                    yield Static(f"📝 {description}")

    def _build_stats_cards(self) -> List[Widget]:
        """Build statistics cards"""
        cards = []
        
        # Get project stats
        sessions = self._get_project_sessions()
        active_sessions = [s for s in sessions if s.get('status') not in ['idle', 'archived']]
        files = get_files_by_project(self.project_name) if self.project_name else []
        context_files = []
        for session in sessions:
            context_files.extend(session.get('files_context', []))
        
        # Sessions card
        sessions_card = Vertical(classes="stat-card")
        sessions_text = Text()
        sessions_text.append(f"{len(active_sessions)}", style="bold")
        sessions_text.append("\nActive Sessions")
        sessions_card.mount(Static(sessions_text))
        cards.append(sessions_card)
        
        # Files card
        files_card = Vertical(classes="stat-card")
        files_text = Text()
        files_text.append(f"{len(files)}", style="bold")
        files_text.append("\nTotal Files")
        files_card.mount(Static(files_text))
        cards.append(files_card)
        
        # Context card
        context_card = Vertical(classes="stat-card")
        context_text = Text()
        context_text.append(f"{len(set(f.get('path', '') for f in context_files))}", style="bold")
        context_text.append("\nIn Context")
        context_card.mount(Static(context_text))
        cards.append(context_card)
        
        # Activity card
        activity_card = Vertical(classes="stat-card")
        activity_text = Text()
        activity_level = self._calculate_activity_level(sessions)
        activity_text.append(get_activity_emoji(activity_level), style="bold")
        activity_text.append(f"\nActivity")
        activity_card.mount(Static(activity_text))
        cards.append(activity_card)
        
        return cards

    def _build_sessions_table(self) -> Widget:
        """Build sessions table"""
        table = DataTable(id="sessions_table", classes="sessions-table")
        table.add_columns("Session", "Status", "Duration", "Progress", "Actions")
        
        sessions = self._get_project_sessions()
        active_sessions = [s for s in sessions if s.get('status') not in ['idle', 'archived']]
        
        if not active_sessions:
            # Show empty state
            empty = Static("No active sessions\n\nClick 'New Session' to get started", 
                          classes="empty-state")
            return empty
        
        for session in active_sessions[:10]:  # Limit to 10 sessions
            status_emoji = self._get_session_status_emoji(session.get('status', 'unknown'))
            
            # Format duration
            duration = session.get('duration', '0m')
            
            # Progress bar text
            progress = session.get('progress', 0)
            progress_text = f"{int(progress * 100)}%"
            
            # Actions
            actions_text = "[View] [Chat]"
            
            table.add_row(
                session.get('name', session.get('id', 'Unknown'))[:20],
                f"{status_emoji} {session.get('status', 'unknown').title()}",
                duration,
                progress_text,
                actions_text,
                key=session.get('id', '')
            )
        
        return table

    def _build_quick_actions(self) -> List[Widget]:
        """Build quick action buttons"""
        actions = []
        
        actions.append(Button("New Session", id="action_new_session", classes="action-btn"))
        actions.append(Button("Import Files", id="action_import_files", classes="action-btn"))
        actions.append(Button("Run Tests", id="action_run_tests", classes="action-btn"))
        actions.append(Button("Deploy", id="action_deploy", classes="action-btn"))
        actions.append(Button("Settings", id="action_settings", classes="action-btn"))
        
        return actions

    def _get_project_sessions(self) -> List[Dict[str, Any]]:
        """Get sessions for current project"""
        if not self.project_name:
            return []
        
        all_sessions = get_sessions()
        return [s for s in all_sessions if s.get('project') == self.project_name]

    def _get_project_status_emoji(self, status: str) -> str:
        """Get emoji for project status"""
        status_map = {
            'active': '🔥',
            'idle': '💤',
            'issues': '⚠️',
            'archived': '📦'
        }
        return status_map.get(status, '❓')

    def _get_session_status_emoji(self, status: str) -> str:
        """Get emoji for session status"""
        status_map = {
            'planning': '🎯',
            'analyzing': '🔍',
            'implementing': '⚙️',
            'reviewing': '👀',
            'stuck': '⚠️',
            'available': '○'
        }
        return status_map.get(status, '❓')

    def _calculate_activity_level(self, sessions: List[Dict[str, Any]]) -> int:
        """Calculate overall project activity level (0-3)"""
        if not sessions:
            return 0
        
        active_sessions = [s for s in sessions if s.get('status') not in ['idle', 'archived']]
        
        if len(active_sessions) >= 3:
            return 3
        elif len(active_sessions) >= 2:
            return 2
        elif len(active_sessions) >= 1:
            return 1
        else:
            return 0

    def set_project(self, project_name: str) -> None:
        """Set the project to display"""
        self.project_name = project_name
        
        # Find project data
        projects = get_projects()
        self.project_data = next((p for p in projects if p.get('name') == project_name), None)
        
        # Rebuild the widget
        self._rebuild_content()

    def _rebuild_content(self) -> None:
        """Rebuild the entire content"""
        # Remove all children and rebuild
        self.remove_children()
        
        # Rebuild composition
        if not self.project_name:
            self.mount(self._build_empty_state())
        else:
            # Project header
            header_container = Vertical(classes="project-header")
            for widget in self._build_project_header():
                header_container.mount(widget)
            self.mount(header_container)
            
            # Statistics cards
            stats_container = Horizontal(classes="project-stats")
            for card in self._build_stats_cards():
                stats_container.mount(card)
            self.mount(stats_container)
            
            # Sessions section
            sessions_container = Vertical(classes="sessions-section")
            sessions_container.mount(Static("🎯 Active Sessions", classes="section-header"))
            sessions_container.mount(self._build_sessions_table())
            self.mount(sessions_container)
            
            # Quick actions
            actions_container = Horizontal(classes="quick-actions")
            for action in self._build_quick_actions():
                actions_container.mount(action)
            self.mount(actions_container)

    def refresh_project_data(self) -> None:
        """Refresh project data"""
        if self.project_name:
            self.set_project(self.project_name)

    def on_data_table_row_selected(self, event: DataTable.RowSelected) -> None:
        """Handle session row selection"""
        table = event.data_table
        row_key = event.row_key
        
        if row_key:
            self.post_message(self.SessionSelected(str(row_key)))

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle action button presses"""
        button_id = event.button.id
        
        if button_id and button_id.startswith("action_"):
            action = button_id.replace("action_", "")
            self.post_message(self.ActionRequested(action, self.project_name))

    def highlight_session(self, session_id: str) -> None:
        """Highlight a specific session in the table"""
        # TODO: Add visual highlighting for session
        pass

    def add_session_to_table(self, session_data: Dict[str, Any]) -> None:
        """Add a new session to the table"""
        if session_data.get('project') == self.project_name:
            self._rebuild_content()

    def remove_session_from_table(self, session_id: str) -> None:
        """Remove a session from the table"""
        self._rebuild_content()

    def update_session_status(self, session_id: str, status: str) -> None:
        """Update session status in the table"""
        self._rebuild_content()