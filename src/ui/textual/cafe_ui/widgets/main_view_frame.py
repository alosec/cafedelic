#!/usr/bin/env python3
"""
MainViewFrame Widget
TabbedContent container for different view types (Session, Project, Dashboard).
"""

from textual.widgets import TabbedContent, TabPane, Static
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget

from .session_view_pane import SessionViewPane
from .project_view_pane import ProjectViewPane


class MainViewFrame(Widget):
    """Main view frame with tabbed content for different view types"""
    
    DEFAULT_CSS = """
    MainViewFrame {
        width: 1fr;
        height: 1fr;
        border: solid $primary;
    }
    
    MainViewFrame TabbedContent {
        width: 1fr;
        height: 1fr;
    }
    
    MainViewFrame TabPane {
        padding: 1;
    }
    """

    class ViewChanged(Message):
        """Message sent when the active view changes"""
        def __init__(self, view_type: str, context: str = "") -> None:
            self.view_type = view_type
            self.context = context
            super().__init__()

    active_session_id: reactive[str] = reactive("")
    active_project_name: reactive[str] = reactive("")

    def compose(self) -> ComposeResult:
        """Compose the main view frame"""
        with TabbedContent(id="main_tabs"):
            # Default dashboard view
            with TabPane("Dashboard", id="dashboard_tab"):
                yield Static("🎯 Cafedelic Task Delegation Platform\n\nSelect a project or session from the tree to begin.", 
                           id="dashboard_content")
            
            # Session views will be added dynamically
            # Project views will be added dynamically

    def show_session_view(self, session_id: str) -> None:
        """Show or switch to session view for given session ID"""
        tab_id = f"session_{session_id}"
        
        # Check if tab already exists
        tabs = self.query_one("#main_tabs", TabbedContent)
        existing_tab = tabs.query(f"#{tab_id}")
        
        if existing_tab:
            # Switch to existing tab
            tabs.active = tab_id
        else:
            # Create new session tab
            with tabs.add_pane(f"Session: {session_id[:8]}", id=tab_id):
                tabs.mount(SessionViewPane(session_id))
            tabs.active = tab_id
        
        self.active_session_id = session_id
        self.post_message(self.ViewChanged("session", session_id))

    def show_project_view(self, project_name: str) -> None:
        """Show or switch to project view for given project"""
        tab_id = f"project_{project_name}"
        
        # Check if tab already exists
        tabs = self.query_one("#main_tabs", TabbedContent)
        existing_tab = tabs.query(f"#{tab_id}")
        
        if existing_tab:
            # Switch to existing tab
            tabs.active = tab_id
        else:
            # Create new project tab
            with tabs.add_pane(f"Project: {project_name}", id=tab_id):
                tabs.mount(ProjectViewPane(project_name))
            tabs.active = tab_id
        
        self.active_project_name = project_name
        self.post_message(self.ViewChanged("project", project_name))

    def close_tab(self, tab_id: str) -> None:
        """Close a specific tab"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        try:
            tab_pane = tabs.query_one(f"#{tab_id}", TabPane)
            tab_pane.remove()
        except Exception:
            # Tab doesn't exist, ignore
            pass

    def close_all_session_tabs(self) -> None:
        """Close all session tabs"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        session_tabs = tabs.query("TabPane").filter(lambda x: x.id and x.id.startswith("session_"))
        for tab in session_tabs:
            tab.remove()

    def close_all_project_tabs(self) -> None:
        """Close all project tabs"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        project_tabs = tabs.query("TabPane").filter(lambda x: x.id and x.id.startswith("project_"))
        for tab in project_tabs:
            tab.remove()

    def get_active_tab_type(self) -> str:
        """Get the type of the currently active tab"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        active_id = tabs.active
        
        if active_id == "dashboard_tab":
            return "dashboard"
        elif active_id and active_id.startswith("session_"):
            return "session"
        elif active_id and active_id.startswith("project_"):
            return "project"
        else:
            return "unknown"

    def get_active_context(self) -> str:
        """Get the context (session_id or project_name) of the active tab"""
        tabs = self.query_one("#main_tabs", TabbedContent)
        active_id = tabs.active
        
        if active_id and active_id.startswith("session_"):
            return active_id.replace("session_", "")
        elif active_id and active_id.startswith("project_"):
            return active_id.replace("project_", "")
        else:
            return ""

    def on_tabbed_content_tab_activated(self, event: TabbedContent.TabActivated) -> None:
        """Handle tab activation"""
        tab_id = event.tab.id
        
        if tab_id == "dashboard_tab":
            self.post_message(self.ViewChanged("dashboard"))
        elif tab_id and tab_id.startswith("session_"):
            session_id = tab_id.replace("session_", "")
            self.active_session_id = session_id
            self.post_message(self.ViewChanged("session", session_id))
        elif tab_id and tab_id.startswith("project_"):
            project_name = tab_id.replace("project_", "")
            self.active_project_name = project_name
            self.post_message(self.ViewChanged("project", project_name))