#!/usr/bin/env python3
"""
SessionNavigator Screen
Full-screen session navigation with file tree, session grid, and slide panel.
"""

from textual.screen import Screen
from textual.containers import Horizontal, Vertical
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widgets import Header, Footer
from typing import Optional

from ..widgets.shared_file_tree import SharedFileTree
from ..widgets.activity_monitor import ActivityMonitor
from ..widgets.slide_panel import SlidePanel
from ..widgets.session_grid import SessionGrid


class SessionNavigator(Screen):
    """Full-screen session navigation interface"""
    
    DEFAULT_CSS = """
    SessionNavigator {
        layout: vertical;
    }
    
    SessionNavigator .main-layout {
        layout: horizontal;
        height: 1fr;
    }
    
    SessionNavigator .left-sidebar {
        width: 25;
        height: 1fr;
    }
    
    SessionNavigator .content-area {
        width: 1fr;
        height: 1fr;
        margin: 0 0 0 1;
    }
    """

    BINDINGS = [
        ("f", "toggle_file_tree", "Toggle File Tree"),
        ("s", "focus_sessions", "Focus Sessions"),
        ("r", "refresh_data", "Refresh"),
        ("n", "new_session", "New Session"),
        ("escape", "back", "Back"),
    ]

    class SessionSelected(Message):
        """Message sent when a session is selected"""
        def __init__(self, session_id: str) -> None:
            self.session_id = session_id
            super().__init__()

    class ProjectViewRequested(Message):
        """Message sent when project view is requested"""
        def __init__(self, project_name: str) -> None:
            self.project_name = project_name
            super().__init__()

    class ChatRequested(Message):
        """Message sent when chat is requested"""
        def __init__(self, session_id: Optional[str] = None) -> None:
            self.session_id = session_id
            super().__init__()

    # Selected session
    selected_session: reactive[str] = reactive("")
    active_filter: reactive[str] = reactive("all")

    def compose(self) -> ComposeResult:
        """Compose the session navigator screen"""
        yield Header()
        
        with Horizontal(classes="main-layout"):
            # Left sidebar with file tree (session context)
            with Vertical(classes="left-sidebar"):
                yield SharedFileTree(context_mode="session", id="file_tree")
            
            # Main content area with session grid
            with Vertical(classes="content-area"):
                yield SessionGrid(id="session_grid")
        
        # Slide panel for session tools
        yield SlidePanel(context_mode="session", id="slide_panel")
        
        # Activity monitor at bottom
        yield ActivityMonitor(id="activity_monitor")
        
        yield Footer()

    def on_mount(self) -> None:
        """Handle screen mount"""
        self.title = "Cafedelic - Session Navigator"
        self.sub_title = "Manage and monitor all sessions"

    def on_shared_file_tree_project_selected(self, event: SharedFileTree.ProjectSelected) -> None:
        """Handle project selection from file tree"""
        # When project is selected in session view, request project view
        self.post_message(self.ProjectViewRequested(event.project_name))

    def on_session_grid_session_selected(self, event: SessionGrid.SessionSelected) -> None:
        """Handle session selection from grid"""
        self.selected_session = event.session_id
        
        # Update slide panel context
        slide_panel = self.query_one("#slide_panel", SlidePanel)
        slide_panel.set_context("session", selected_session=event.session_id)
        
        # Expand slide panel to show session tools
        slide_panel.expand_panel()
        
        self.post_message(self.SessionSelected(event.session_id))

    def on_session_grid_session_action_requested(self, event: SessionGrid.SessionActionRequested) -> None:
        """Handle session action requests from grid"""
        if event.action == "view":
            self.post_message(self.SessionSelected(event.session_id))
        elif event.action == "chat":
            self.post_message(self.ChatRequested(event.session_id))

    def on_session_grid_filter_changed(self, event: SessionGrid.FilterChanged) -> None:
        """Handle session filter changes"""
        self.active_filter = event.filter_type
        
        # Update file tree to show session counts for this filter
        file_tree = self.query_one("#file_tree", SharedFileTree)
        file_tree._rebuild_tree()

    def on_slide_panel_action_requested(self, event: SlidePanel.ActionRequested) -> None:
        """Handle action requests from slide panel"""
        action = event.action
        context = event.context
        
        if action == "session_new":
            self._handle_new_session()
        elif action == "session_bulk_archive":
            self._handle_bulk_archive()
        elif action == "session_health":
            self._handle_health_check()
        elif action == "session_export":
            self._handle_export_sessions()
        elif action == "session_analytics":
            self._handle_session_analytics()
        elif action == "session_cleanup":
            self._handle_cleanup_idle()
        elif action.startswith("filter_"):
            filter_type = action.replace("filter_", "")
            self._set_session_filter(filter_type)
        elif action.startswith("sort_"):
            sort_type = action.replace("sort_", "")
            self._set_session_sort(sort_type)

    def on_activity_monitor_activity_cleared(self, event: ActivityMonitor.ActivityCleared) -> None:
        """Handle activity cleared"""
        pass

    def on_activity_monitor_activity_filtered(self, event: ActivityMonitor.ActivityFiltered) -> None:
        """Handle activity filter change"""
        pass

    def _handle_new_session(self) -> None:
        """Handle new session creation"""
        # TODO: Open new session dialog
        pass

    def _handle_bulk_archive(self) -> None:
        """Handle bulk session archival"""
        # TODO: Show bulk archive dialog
        pass

    def _handle_health_check(self) -> None:
        """Handle session health check"""
        # TODO: Run health check on all sessions
        pass

    def _handle_export_sessions(self) -> None:
        """Handle session data export"""
        # TODO: Export session data
        pass

    def _handle_session_analytics(self) -> None:
        """Handle session analytics view"""
        # TODO: Show session analytics
        pass

    def _handle_cleanup_idle(self) -> None:
        """Handle cleanup of idle sessions"""
        # TODO: Cleanup idle sessions
        pass

    def _set_session_filter(self, filter_type: str) -> None:
        """Set session filter"""
        session_grid = self.query_one("#session_grid", SessionGrid)
        session_grid.set_filter(filter_type)
        self.active_filter = filter_type

    def _set_session_sort(self, sort_type: str) -> None:
        """Set session sorting"""
        session_grid = self.query_one("#session_grid", SessionGrid)
        session_grid.set_sort(sort_type)

    def action_toggle_file_tree(self) -> None:
        """Toggle file tree visibility"""
        file_tree = self.query_one("#file_tree", SharedFileTree)
        if file_tree.styles.display == "none":
            file_tree.styles.display = "block"
        else:
            file_tree.styles.display = "none"

    def action_focus_sessions(self) -> None:
        """Focus on the session grid"""
        session_grid = self.query_one("#session_grid", SessionGrid)
        session_grid.focus()

    def action_refresh_data(self) -> None:
        """Refresh all data"""
        # Refresh file tree
        file_tree = self.query_one("#file_tree", SharedFileTree)
        file_tree._rebuild_tree()
        
        # Refresh session grid
        session_grid = self.query_one("#session_grid", SessionGrid)
        session_grid.refresh_sessions()
        
        # Refresh activity monitor
        activity_monitor = self.query_one("#activity_monitor", ActivityMonitor)
        activity_monitor._refresh_activities()

    def action_new_session(self) -> None:
        """Create new session"""
        self._handle_new_session()

    def action_back(self) -> None:
        """Return to previous screen"""
        self.app.pop_screen()

    def set_filter(self, filter_type: str) -> None:
        """Set the active filter programmatically"""
        self._set_session_filter(filter_type)

    def set_sort(self, sort_type: str) -> None:
        """Set the active sort programmatically"""
        self._set_session_sort(sort_type)

    def select_session(self, session_id: str) -> None:
        """Select a specific session programmatically"""
        session_grid = self.query_one("#session_grid", SessionGrid)
        session_grid.select_session(session_id)
        self.selected_session = session_id

    def get_selected_session(self) -> str:
        """Get the currently selected session"""
        return self.selected_session

    def get_active_filter(self) -> str:
        """Get the currently active filter"""
        return self.active_filter

    def refresh_all(self) -> None:
        """Refresh all components"""
        self.action_refresh_data()

    def show_session_creation_dialog(self) -> None:
        """Show dialog for creating new session"""
        # TODO: Implement session creation dialog
        self.app.notify("Session creation dialog - To be implemented", severity="info")

    def show_bulk_actions_dialog(self) -> None:
        """Show dialog for bulk session actions"""
        # TODO: Implement bulk actions dialog
        self.app.notify("Bulk actions dialog - To be implemented", severity="info")

    def show_session_health_report(self) -> None:
        """Show session health report"""
        stats = self.get_session_statistics()
        health_text = f"Session Health Report:\n"
        health_text += f"• Total Sessions: {stats['total']}\n"
        health_text += f"• Active Sessions: {stats['active']}\n" 
        health_text += f"• Stuck Sessions: {stats['stuck']}\n"
        health_text += f"• Idle Sessions: {stats['idle']}"
        self.app.notify(health_text, severity="info")

    def bulk_archive_idle_sessions(self) -> None:
        """Archive all idle sessions"""
        # TODO: Implement bulk archive
        self.app.notify("Bulk archive idle sessions - To be implemented", severity="info")

    def run_health_check_all_sessions(self) -> None:
        """Run health check on all sessions"""
        # TODO: Implement health check
        self.app.notify("Running health check on all sessions - To be implemented", severity="info")

    def export_session_report(self) -> None:
        """Export session data report"""
        # TODO: Implement session export
        self.app.notify("Export session report - To be implemented", severity="info")

    def cleanup_stuck_sessions(self) -> None:
        """Cleanup sessions that are stuck"""
        # TODO: Implement stuck session cleanup
        self.app.notify("Cleanup stuck sessions - To be implemented", severity="info")

    def show_session_analytics(self) -> None:
        """Show session analytics and insights"""
        stats = self.get_session_statistics()
        analytics_text = f"Session Analytics:\n"
        analytics_text += f"• Active Rate: {(stats['active']/stats['total']*100):.1f}% ({stats['active']}/{stats['total']})\n"
        analytics_text += f"• Stuck Rate: {(stats['stuck']/stats['total']*100):.1f}% ({stats['stuck']}/{stats['total']})\n"
        analytics_text += f"• Current Filter: {self.active_filter.title()}"
        self.app.notify(analytics_text, severity="info")

    def get_session_statistics(self) -> dict:
        """Get session statistics"""
        session_grid = self.query_one("#session_grid", SessionGrid)
        return session_grid.get_session_count()

    def add_session(self, session_data: dict) -> None:
        """Add a new session to the grid"""
        session_grid = self.query_one("#session_grid", SessionGrid)
        session_grid.add_session(session_data)

    def remove_session(self, session_id: str) -> None:
        """Remove a session from the grid"""
        session_grid = self.query_one("#session_grid", SessionGrid)
        session_grid.remove_session(session_id)

    def update_session_status(self, session_id: str, status: str) -> None:
        """Update session status"""
        # TODO: Update session in grid
        pass