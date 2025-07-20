#!/usr/bin/env python3
"""
ProjectNavigator Screen
Full-screen project navigation with file tree, project overview, and slide panel.
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
from ..widgets.project_overview import ProjectOverview


class ProjectNavigator(Screen):
    """Full-screen project navigation interface"""
    
    DEFAULT_CSS = """
    ProjectNavigator {
        layout: vertical;
    }
    
    ProjectNavigator .main-layout {
        layout: horizontal;
        height: 1fr;
    }
    
    ProjectNavigator .left-sidebar {
        width: 25;
        height: 1fr;
    }
    
    ProjectNavigator .content-area {
        width: 1fr;
        height: 1fr;
        margin: 0 0 0 1;
    }
    """

    BINDINGS = [
        ("f", "toggle_file_tree", "Toggle File Tree"),
        ("p", "focus_projects", "Focus Projects"),
        ("r", "refresh_data", "Refresh"),
        ("escape", "back", "Back"),
    ]

    class ProjectSelected(Message):
        """Message sent when a project is selected"""
        def __init__(self, project_name: str) -> None:
            self.project_name = project_name
            super().__init__()

    class SessionRequested(Message):
        """Message sent when session view is requested"""
        def __init__(self, session_id: str) -> None:
            self.session_id = session_id
            super().__init__()

    class ChatRequested(Message):
        """Message sent when chat is requested"""
        def __init__(self, session_id: Optional[str] = None) -> None:
            self.session_id = session_id
            super().__init__()

    # Selected project
    selected_project: reactive[str] = reactive("")

    def compose(self) -> ComposeResult:
        """Compose the project navigator screen"""
        yield Header()
        
        with Horizontal(classes="main-layout"):
            # Left sidebar with file tree
            with Vertical(classes="left-sidebar"):
                yield SharedFileTree(context_mode="project", id="file_tree")
            
            # Main content area with project overview
            with Vertical(classes="content-area"):
                yield ProjectOverview(id="project_overview")
        
        # Slide panel for project tools
        yield SlidePanel(context_mode="project", id="slide_panel")
        
        # Activity monitor at bottom
        yield ActivityMonitor(id="activity_monitor")
        
        yield Footer()

    def on_mount(self) -> None:
        """Handle screen mount"""
        self.title = "Cafedelic - Project Navigator"
        self.sub_title = "Manage projects and sessions"

    def on_shared_file_tree_project_selected(self, event: SharedFileTree.ProjectSelected) -> None:
        """Handle project selection from file tree"""
        self.selected_project = event.project_name
        
        # Update project overview
        overview = self.query_one("#project_overview", ProjectOverview)
        overview.set_project(event.project_name)
        
        # Update slide panel context
        slide_panel = self.query_one("#slide_panel", SlidePanel)
        slide_panel.set_context("project", selected_project=event.project_name)
        
        # Expand slide panel to show project tools
        slide_panel.expand_panel()
        
        self.post_message(self.ProjectSelected(event.project_name))

    def on_shared_file_tree_file_selected(self, event: SharedFileTree.FileSelected) -> None:
        """Handle file selection from file tree"""
        # Could open file in editor or show file details
        pass

    def on_project_overview_session_selected(self, event) -> None:
        """Handle session selection from project overview"""
        if hasattr(event, 'session_id'):
            self.post_message(self.SessionRequested(event.session_id))

    def on_project_overview_chat_requested(self, event) -> None:
        """Handle chat request from project overview"""
        session_id = getattr(event, 'session_id', None)
        self.post_message(self.ChatRequested(session_id))

    def on_slide_panel_action_requested(self, event: SlidePanel.ActionRequested) -> None:
        """Handle action requests from slide panel"""
        action = event.action
        context = event.context
        
        if action == "project_new_session":
            self._handle_new_session()
        elif action == "project_archive":
            self._handle_archive_project()
        elif action == "project_settings":
            self._handle_project_settings()
        elif action == "project_deploy":
            self._handle_deploy_project()
        elif action == "project_analytics":
            self._handle_project_analytics()
        elif action == "project_export":
            self._handle_export_project()

    def on_activity_monitor_activity_cleared(self, event: ActivityMonitor.ActivityCleared) -> None:
        """Handle activity cleared"""
        pass

    def on_activity_monitor_activity_filtered(self, event: ActivityMonitor.ActivityFiltered) -> None:
        """Handle activity filter change"""
        pass

    def _handle_new_session(self) -> None:
        """Handle new session creation"""
        if self.selected_project:
            # TODO: Open new session dialog
            pass

    def _handle_archive_project(self) -> None:
        """Handle project archival"""
        if self.selected_project:
            # TODO: Confirm and archive project
            pass

    def _handle_project_settings(self) -> None:
        """Handle project settings"""
        if self.selected_project:
            # TODO: Open project settings dialog
            pass

    def _handle_deploy_project(self) -> None:
        """Handle project deployment"""
        if self.selected_project:
            # TODO: Start deployment process
            pass

    def _handle_project_analytics(self) -> None:
        """Handle project analytics view"""
        if self.selected_project:
            # TODO: Show analytics for project
            pass

    def _handle_export_project(self) -> None:
        """Handle project data export"""
        if self.selected_project:
            # TODO: Export project data
            pass

    def action_toggle_file_tree(self) -> None:
        """Toggle file tree visibility"""
        file_tree = self.query_one("#file_tree", SharedFileTree)
        if file_tree.styles.display == "none":
            file_tree.styles.display = "block"
        else:
            file_tree.styles.display = "none"

    def action_focus_projects(self) -> None:
        """Focus on the file tree"""
        file_tree = self.query_one("#file_tree", SharedFileTree)
        file_tree.focus()

    def action_refresh_data(self) -> None:
        """Refresh all data"""
        # Refresh file tree
        file_tree = self.query_one("#file_tree", SharedFileTree)
        file_tree._rebuild_tree()
        
        # Refresh project overview
        if self.selected_project:
            overview = self.query_one("#project_overview", ProjectOverview)
            overview.refresh_project_data()
        
        # Refresh activity monitor
        activity_monitor = self.query_one("#activity_monitor", ActivityMonitor)
        activity_monitor._refresh_activities()

    def action_back(self) -> None:
        """Return to previous screen"""
        self.app.pop_screen()

    def set_project(self, project_name: str) -> None:
        """Set the active project programmatically"""
        self.selected_project = project_name
        
        # Update all components
        file_tree = self.query_one("#file_tree", SharedFileTree)
        file_tree.highlight_project(project_name)
        
        overview = self.query_one("#project_overview", ProjectOverview)
        overview.set_project(project_name)
        
        slide_panel = self.query_one("#slide_panel", SlidePanel)
        slide_panel.set_context("project", selected_project=project_name)

    def get_selected_project(self) -> str:
        """Get the currently selected project"""
        return self.selected_project

    def refresh_all(self) -> None:
        """Refresh all components"""
        self.action_refresh_data()

    def show_project_creation_dialog(self) -> None:
        """Show dialog for creating new project"""
        # TODO: Implement project creation dialog
        self.app.notify("Project creation dialog - To be implemented", severity="info")

    def show_project_import_dialog(self) -> None:
        """Show dialog for importing existing project"""
        # TODO: Implement project import dialog
        self.app.notify("Project import dialog - To be implemented", severity="info")

    def show_session_creation_dialog(self) -> None:
        """Show dialog for creating new session for current project"""
        if self.selected_project:
            # TODO: Implement session creation dialog
            self.app.notify(f"Create session for {self.selected_project} - To be implemented", severity="info")
        else:
            self.app.notify("No project selected", severity="warning")

    def archive_current_project(self) -> None:
        """Archive the currently selected project"""
        if self.selected_project:
            # TODO: Implement project archival
            self.app.notify(f"Archive project {self.selected_project} - To be implemented", severity="info")
        else:
            self.app.notify("No project selected", severity="warning")

    def show_project_settings(self) -> None:
        """Show settings for the current project"""
        if self.selected_project:
            # TODO: Implement project settings
            self.app.notify(f"Settings for {self.selected_project} - To be implemented", severity="info")
        else:
            self.app.notify("No project selected", severity="warning")

    def deploy_current_project(self) -> None:
        """Deploy the currently selected project"""
        if self.selected_project:
            # TODO: Implement project deployment
            self.app.notify(f"Deploy {self.selected_project} - To be implemented", severity="info")
        else:
            self.app.notify("No project selected", severity="warning")

    def show_project_analytics(self) -> None:
        """Show analytics for the current project"""
        if self.selected_project:
            # TODO: Implement project analytics
            self.app.notify(f"Analytics for {self.selected_project} - To be implemented", severity="info")
        else:
            self.app.notify("No project selected", severity="warning")