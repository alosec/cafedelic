#!/usr/bin/env python3
"""
SlidePanel Widget
Context-sensitive slide-in panel with flexible content based on navigation mode.
"""

from textual.widgets import Static, Button, Label, TabPane
from textual.containers import Horizontal, Vertical, Container
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget
from textual.events import Mount
from rich.text import Text
from typing import Optional, Dict, Any, List


class SlidePanel(Widget):
    """Context-sensitive slide-in panel widget"""
    
    DEFAULT_CSS = """
    SlidePanel {
        dock: bottom;
        height: 6;
        background: $panel;
        border: solid $primary;
        margin: 1 0 0 0;
    }
    
    SlidePanel.collapsed {
        height: 1;
    }
    
    SlidePanel .panel-header {
        dock: top;
        height: 1;
        background: $accent;
        padding: 0 1;
        layout: horizontal;
    }
    
    SlidePanel .panel-content {
        height: 1fr;
        padding: 1;
        layout: horizontal;
        overflow: auto;
    }
    
    SlidePanel .toggle-btn {
        background: $accent;
        color: $text;
        min-width: 3;
        max-height: 1;
        margin: 0 1 0 0;
    }
    
    SlidePanel .toggle-btn:hover {
        background: $accent 80%;
    }
    
    SlidePanel .action-group {
        margin: 0 2 0 0;
        layout: vertical;
    }
    
    SlidePanel .action-group-title {
        text-align: center;
        margin: 0 0 1 0;
    }
    
    SlidePanel .action-buttons {
        layout: horizontal;
    }
    
    SlidePanel .action-btn {
        background: $primary;
        color: $text;
        margin: 0 1;
        min-width: 12;
        max-height: 1;
    }
    
    SlidePanel .action-btn:hover {
        background: $primary 80%;
    }
    
    SlidePanel .context-info {
        background: $surface;
        padding: 1;
        margin: 0 1 0 0;
        border: solid $accent;
        min-width: 25;
    }
    
    SlidePanel .bold-text {
        text-style: bold;
    }
    
    SlidePanel .dim-text {
        text-style: dim;
    }
    """

    class ActionRequested(Message):
        """Message sent when an action is requested"""
        def __init__(self, action: str, context: Dict[str, Any]) -> None:
            self.action = action
            self.context = context
            super().__init__()

    class PanelToggled(Message):
        """Message sent when panel is toggled"""
        def __init__(self, expanded: bool) -> None:
            self.expanded = expanded
            super().__init__()

    # Panel state
    expanded: reactive[bool] = reactive(True)
    context_mode: reactive[str] = reactive("project")  # project, session, chat
    selected_project: reactive[str] = reactive("")
    selected_session: reactive[str] = reactive("")
    
    def __init__(self, context_mode: str = "project", **kwargs):
        super().__init__(**kwargs)
        self.context_mode = context_mode

    def compose(self) -> ComposeResult:
        """Compose the slide panel"""
        # Header with toggle
        with Horizontal(classes="panel-header"):
            yield Button("⬇" if self.expanded else "⬆", id="toggle_panel", classes="toggle-btn")
            yield Static(self._get_header_text(), id="panel_title")
        
        # Content (only shown when expanded)
        if self.expanded:
            with Container(classes="panel-content"):
                yield from self._build_panel_content()

    def _get_header_text(self) -> str:
        """Get header text based on context mode"""
        if self.context_mode == "project":
            project_name = self.selected_project or "No Project"
            return f"⬆ Project Tools: {project_name}"
        elif self.context_mode == "session":
            return "⬆ Session Management Tools"
        elif self.context_mode == "chat":
            return "⬆ Chat & Delegation Tools"
        else:
            return "⬆ Tools Panel"

    def _build_panel_content(self) -> ComposeResult:
        """Build panel content based on context mode"""
        with Horizontal():
            if self.context_mode == "project":
                yield from self._build_project_tools()
                yield from self._build_project_info()
            elif self.context_mode == "session":
                yield from self._build_session_tools()
                yield from self._build_session_filters()
            elif self.context_mode == "chat":
                yield from self._build_chat_tools()
                yield from self._build_delegation_tools()

    def _build_project_tools(self) -> ComposeResult:
        """Build project-specific tools"""
        with Vertical(classes="action-group"):
            yield Static("Project Actions", classes="action-group-title")
            
            with Horizontal(classes="action-buttons"):
                yield Button("New Session", id="project_new_session", classes="action-btn")
                yield Button("Archive Project", id="project_archive", classes="action-btn")
                yield Button("Project Settings", id="project_settings", classes="action-btn")
            
            with Horizontal(classes="action-buttons"):
                yield Button("Deploy to Staging", id="project_deploy", classes="action-btn")
                yield Button("View Analytics", id="project_analytics", classes="action-btn")
                yield Button("Export Data", id="project_export", classes="action-btn")

    def _build_project_info(self) -> ComposeResult:
        """Build project information display"""
        with Vertical(classes="context-info"):
            if self.selected_project:
                yield Static(f"📁 Project: {self.selected_project}", classes="bold-text")
                yield Static("Status: 🔄 Active")
                yield Static("Sessions: 3 active, 2 idle")
                yield Static("Files: 127 total, 15 in context")
                yield Static("")
                yield Static("Quick Actions:", classes="bold-text")
                yield Static("• Add all modified files to new session")
                yield Static("• Create session from git branch")
                yield Static("• Auto-organize sessions by feature")
            else:
                yield Static("No project selected", classes="dim-text")
                yield Static("Select a project from the file tree to see tools")

    def _build_session_tools(self) -> ComposeResult:
        """Build session management tools"""
        with Vertical(classes="action-group"):
            yield Static("Session Actions", classes="action-group-title")
            
            with Horizontal(classes="action-buttons"):
                yield Button("New Session", id="session_new", classes="action-btn")
                yield Button("Bulk Archive", id="session_bulk_archive", classes="action-btn")
                yield Button("Health Check", id="session_health", classes="action-btn")
            
            with Horizontal(classes="action-buttons"):
                yield Button("Export Report", id="session_export", classes="action-btn")
                yield Button("Session Analytics", id="session_analytics", classes="action-btn")
                yield Button("Cleanup Idle", id="session_cleanup", classes="action-btn")

    def _build_session_filters(self) -> ComposeResult:
        """Build session filters and sorting"""
        with Vertical(classes="context-info"):
            yield Static("Filters & Sorting", classes="bold-text")
            yield Static("")
            
            with Horizontal():
                yield Button("All", id="filter_all", classes="action-btn")
                yield Button("Active", id="filter_active", classes="action-btn")
                yield Button("Stuck", id="filter_stuck", classes="action-btn")
                yield Button("Recent", id="filter_recent", classes="action-btn")
            
            yield Static("")
            
            with Horizontal():
                yield Button("Activity", id="sort_activity", classes="action-btn")
                yield Button("Duration", id="sort_duration", classes="action-btn")
                yield Button("Progress", id="sort_progress", classes="action-btn")

    def _build_chat_tools(self) -> ComposeResult:
        """Build chat and delegation tools"""
        with Vertical(classes="action-group"):
            yield Static("Chat Actions", classes="action-group-title")
            
            with Horizontal(classes="action-buttons"):
                yield Button("Quick Delegate", id="chat_delegate", classes="action-btn")
                yield Button("Broadcast Task", id="chat_broadcast", classes="action-btn")
                yield Button("Session History", id="chat_history", classes="action-btn")

    def _build_delegation_tools(self) -> ComposeResult:
        """Build delegation-specific tools"""
        with Vertical(classes="context-info"):
            yield Static("Delegation Options", classes="bold-text")
            yield Static("")
            yield Static("Target Session Types:")
            
            with Horizontal():
                yield Button("Planning", id="target_planning", classes="action-btn")
                yield Button("Implementation", id="target_implementation", classes="action-btn")
            
            yield Static("")
            yield Static("Quick Templates:")
            yield Static("• Implement feature X")
            yield Static("• Fix bug in component Y")
            yield Static("• Add tests for module Z")

    def set_context(self, mode: str, selected_project: str = "", selected_session: str = "") -> None:
        """Set panel context and rebuild content"""
        self.context_mode = mode
        self.selected_project = selected_project
        self.selected_session = selected_session
        
        # Update header
        header = self.query_one("#panel_title", Static)
        header.update(self._get_header_text())
        
        # Rebuild content if expanded
        if self.expanded:
            self._rebuild_content()

    def toggle_panel(self) -> None:
        """Toggle panel expanded/collapsed state"""
        self.expanded = not self.expanded
        
        # Update toggle button
        toggle_btn = self.query_one("#toggle_panel", Button)
        toggle_btn.label = "⬇" if self.expanded else "⬆"
        
        # Update styles
        if self.expanded:
            self.remove_class("collapsed")
            # Add content
            content_container = Container(classes="panel-content")
            content_container.mount(self._build_panel_content())
            self.mount(content_container)
        else:
            self.add_class("collapsed")
            # Remove content
            try:
                content = self.query_one(".panel-content")
                content.remove()
            except:
                pass
        
        self.post_message(self.PanelToggled(self.expanded))

    def _rebuild_content(self) -> None:
        """Rebuild panel content"""
        try:
            content = self.query_one(".panel-content", Container)
            content.remove_children()
            content.mount(self._build_panel_content())
        except:
            # Content doesn't exist, panel is collapsed
            pass

    def expand_panel(self) -> None:
        """Expand the panel"""
        if not self.expanded:
            self.toggle_panel()

    def collapse_panel(self) -> None:
        """Collapse the panel"""
        if self.expanded:
            self.toggle_panel()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle button presses"""
        button_id = event.button.id
        
        if button_id == "toggle_panel":
            self.toggle_panel()
            return
        
        # Extract action and send message
        if button_id:
            context = {
                "mode": self.context_mode,
                "project": self.selected_project,
                "session": self.selected_session
            }
            self.post_message(self.ActionRequested(button_id, context))

    def add_quick_action(self, action_id: str, label: str, group: str = "custom") -> None:
        """Add a custom quick action button"""
        # TODO: Implement dynamic action addition
        pass

    def set_project_info(self, project_data: Dict[str, Any]) -> None:
        """Update project information display"""
        if self.context_mode == "project" and self.expanded:
            self._rebuild_content()

    def set_session_count(self, active: int, idle: int) -> None:
        """Update session count display"""
        if self.context_mode == "session" and self.expanded:
            self._rebuild_content()