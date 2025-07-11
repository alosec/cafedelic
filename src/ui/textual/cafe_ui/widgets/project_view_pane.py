#!/usr/bin/env python3
"""
ProjectViewPane Widget
Single project overview with session health and file activity summary.
"""

from textual.widgets import Static, Button, DataTable
from textual.containers import Vertical, Horizontal
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget
from rich.text import Text

from ..data.mock_data import (
    get_project_by_name, get_sessions, get_files_by_project,
    get_activity_emoji, MockProject
)


class ProjectViewPane(Widget):
    """Project view pane for single project overview"""
    
    DEFAULT_CSS = """
    ProjectViewPane {
        width: 1fr;
        height: 1fr;
        layout: vertical;
    }
    
    ProjectViewPane .project-header {
        dock: top;
        height: 4;
        background: $panel;
        padding: 1;
        border: solid $primary;
    }
    
    ProjectViewPane .content-area {
        height: 1fr;
        layout: horizontal;
    }
    
    ProjectViewPane .sessions-panel {
        width: 1fr;
        border: solid $primary;
        margin: 0 1 0 0;
    }
    
    ProjectViewPane .files-panel {
        width: 1fr;
        border: solid $primary;
    }
    
    ProjectViewPane .intelligence-panel {
        dock: bottom;
        height: 8;
        background: $surface;
        border: solid $accent;
        padding: 1;
    }
    
    ProjectViewPane .action-btn {
        background: $accent;
        color: $text;
        margin: 0 1;
        padding: 0 1;
        border: none;
        min-width: 0;
    }
    
    ProjectViewPane .action-btn:hover {
        background: $accent 80%;
    }
    """

    class SessionRequested(Message):
        """Message sent when a session is requested to be opened"""
        def __init__(self, session_id: str) -> None:
            self.session_id = session_id
            super().__init__()

    project_name: reactive[str] = reactive("")
    project_data: reactive[MockProject | None] = reactive(None)

    def __init__(self, project_name: str, **kwargs):
        super().__init__(**kwargs)
        self.project_name = project_name
        self.project_data = get_project_by_name(project_name)

    def compose(self) -> ComposeResult:
        """Compose the project view pane"""
        if not self.project_data:
            yield Static("❌ Project not found", id="error_message")
            return

        project = self.project_data

        # Project header
        with Vertical(classes="project-header"):
            header_text = Text()
            header_text.append(f"📁 Project: {project.name}", style="bold")
            activity = get_activity_emoji(project.activity_level)
            header_text.append(f"    {activity}")
            
            status_color = "green" if project.status == "active" else "red" if project.status == "issues" else "dim"
            header_text.append(f"    Status: {project.status.title()}", style=status_color)
            yield Static(header_text)
            
            path_text = Text()
            path_text.append(f"Path: {project.path}")
            yield Static(path_text)
            
            stats_text = Text()
            stats_text.append(f"Sessions: {len(project.sessions)} | ")
            stats_text.append(f"Files: {len(get_files_by_project(project.name))} | ")
            stats_text.append(f"Activity: {activity}")
            yield Static(stats_text)

        # Content area
        with Horizontal(classes="content-area"):
            # Sessions panel
            with Vertical(classes="sessions-panel"):
                yield Static("🎯 Session Overview", style="bold")
                yield self._build_sessions_table()

            # Files panel
            with Vertical(classes="files-panel"):
                yield Static("📁 File Activity Summary", style="bold")
                yield self._build_files_summary()

        # Intelligence panel
        with Vertical(classes="intelligence-panel"):
            yield Static("🧠 Project Intelligence Insights", style="bold")
            yield self._build_intelligence_summary()

    def _build_sessions_table(self) -> Widget:
        """Build sessions overview table"""
        if not self.project_data:
            return Static("No project data")

        # Get sessions for this project
        all_sessions = get_sessions()
        project_sessions = [s for s in all_sessions if s.project == self.project_name]

        if not project_sessions:
            return Static("No active sessions")

        # Create table
        table = DataTable()
        table.add_columns("Session", "Status", "Task", "Progress", "Duration", "Actions")
        
        for session in project_sessions:
            status_emoji = self._get_session_status_emoji(session.status)
            progress_bar = self._create_progress_bar(session.progress)
            
            table.add_row(
                session.name,
                f"{status_emoji} {session.status.title()}",
                session.task[:30] + "..." if len(session.task) > 30 else session.task,
                progress_bar,
                session.duration,
                "[Open]"
            )
        
        return table

    def _build_files_summary(self) -> Widget:
        """Build file activity summary"""
        container = Vertical()
        
        files = get_files_by_project(self.project_name)
        
        if not files:
            container.mount(Static("No files found"))
            return container

        # Group by activity level
        high_activity = [f for f in files if f.activity_level == 3]
        medium_activity = [f for f in files if f.activity_level == 2]
        low_activity = [f for f in files if f.activity_level == 1]
        no_activity = [f for f in files if f.activity_level == 0]

        if high_activity:
            container.mount(Static("📈 High Activity Files:", style="bold red"))
            for file in high_activity:
                activity = get_activity_emoji(file.activity_level)
                file_text = f"  📄 {file.name} {activity} [{file.status}]"
                container.mount(Static(file_text))
            container.mount(Static(""))

        if medium_activity:
            container.mount(Static("📊 Medium Activity Files:", style="bold yellow"))
            for file in medium_activity:
                activity = get_activity_emoji(file.activity_level)
                file_text = f"  📄 {file.name} {activity} [{file.status}]"
                container.mount(Static(file_text))
            container.mount(Static(""))

        # Summary stats
        context_files = [f for f in files if f.in_context]
        modified_files = [f for f in files if f.status == "modified"]
        
        summary_text = Text()
        summary_text.append(f"Total: {len(files)} files | ")
        summary_text.append(f"In Context: {len(context_files)} | ")
        summary_text.append(f"Modified: {len(modified_files)}")
        container.mount(Static(summary_text))

        return container

    def _build_intelligence_summary(self) -> Widget:
        """Build intelligence insights summary"""
        container = Vertical()
        
        if not self.project_data:
            return container

        project = self.project_data
        
        # Mock intelligence insights
        insights = []
        
        if project.status == "active":
            insights.append("🔥 High productivity detected - 15 meaningful changes in past hour")
            insights.append("🤝 Coordination opportunity: auth-refactor ↔ database-opt sessions")
            
        elif project.status == "issues":
            insights.append("⚠️ Stuck session detected - may need manual intervention")
            insights.append("🔧 Suggested action: Debug session with error analysis")
            
        elif project.status == "idle":
            insights.append("💤 Project has been idle - consider resuming development")
            insights.append("📋 Suggested action: Review pending tasks and create new session")

        for insight in insights:
            container.mount(Static(insight))

        if insights:
            container.mount(Static(""))

        # Quick actions
        actions_container = Horizontal()
        actions_container.mount(Button("[+ New Session]", classes="action-btn"))
        actions_container.mount(Button("[📊 Analytics]", classes="action-btn"))
        actions_container.mount(Button("[🔧 Debug]", classes="action-btn"))
        actions_container.mount(Button("[📋 Export]", classes="action-btn"))
        container.mount(actions_container)

        return container

    def _get_session_status_emoji(self, status: str) -> str:
        """Get emoji for session status"""
        status_map = {
            'planning': '🧠',
            'analyzing': '🔍',
            'implementing': '⚡',
            'reviewing': '👀',
            'stuck': '⚠️',
            'available': '💤'
        }
        return status_map.get(status, '❓')

    def _create_progress_bar(self, progress: float) -> str:
        """Create a text-based progress bar"""
        filled = int(progress * 10)
        empty = 10 - filled
        return "█" * filled + "░" * empty + f" {int(progress * 100)}%"

    def on_data_table_cell_selected(self, event: DataTable.CellSelected) -> None:
        """Handle session table cell selection"""
        if event.coordinate.column == 5:  # Actions column
            # Get the session from the row
            table = event.data_table
            row_key = event.coordinate.row
            
            # Get sessions for this project
            all_sessions = get_sessions()
            project_sessions = [s for s in all_sessions if s.project == self.project_name]
            
            if row_key < len(project_sessions):
                session = project_sessions[row_key]
                self.post_message(self.SessionRequested(session.id))

    def refresh_project_data(self) -> None:
        """Refresh project data and update display"""
        self.project_data = get_project_by_name(self.project_name)