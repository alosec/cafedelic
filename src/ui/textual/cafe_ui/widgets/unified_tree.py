#!/usr/bin/env python3
"""
UnifiedTree Widget
Context-aware tree widget that adapts display based on navigation mode:
- project: Shows projects with session counts
- session: Shows sessions grouped by project  
- files: Shows file tree for selected project
"""

from textual.widgets import Tree, Static, Button
from textual.containers import Vertical, ScrollableContainer
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget
from rich.text import Text
from typing import Optional, Dict, List, Any
from pathlib import Path

from ..data.claude_code_data import get_projects, get_sessions, get_files_by_project, get_context_files_by_session


class UnifiedTree(Widget):
    """Unified tree widget that adapts to navigation context"""
    
    DEFAULT_CSS = """
    UnifiedTree {
        width: 25;
        height: 1fr;
        border: solid $primary;
        background: $surface;
    }
    
    UnifiedTree .tree-header {
        dock: top;
        height: 3;
        background: $panel;
        padding: 1;
        text-align: center;
    }
    
    UnifiedTree .tree-content {
        height: 1fr;
        padding: 1;
    }
    
    UnifiedTree .tree-controls {
        dock: bottom;
        height: 3;
        background: $panel;
        padding: 1;
        layout: horizontal;
    }
    
    UnifiedTree .control-btn {
        background: $accent;
        color: $text;
        margin: 0 1;
        min-width: 8;
    }
    
    UnifiedTree .control-btn:hover {
        background: $accent 80%;
    }
    
    UnifiedTree Tree {
        height: 1fr;
    }
    
    UnifiedTree .tree-item {
        padding: 0;
    }
    
    UnifiedTree .tree-item:hover {
        background: $primary 20%;
    }
    """

    class ProjectSelected(Message):
        """Message sent when a project is selected"""
        def __init__(self, project_name: str) -> None:
            self.project_name = project_name
            super().__init__()

    class SessionSelected(Message):
        """Message sent when a session is selected"""
        def __init__(self, session_id: str, project_name: str) -> None:
            self.session_id = session_id
            self.project_name = project_name
            super().__init__()

    class FileSelected(Message):
        """Message sent when a file is selected"""
        def __init__(self, file_path: str, project_name: str) -> None:
            self.file_path = file_path
            self.project_name = project_name
            super().__init__()

    class ChatRequested(Message):
        """Message sent when chat is requested for a session"""
        def __init__(self, session_id: str) -> None:
            self.session_id = session_id
            super().__init__()

    class RefreshRequested(Message):
        """Message sent when refresh is requested"""
        def __init__(self) -> None:
            super().__init__()

    # Context modes: 'project', 'session', 'files'
    context_mode: reactive[str] = reactive("project")
    selected_project: reactive[str] = reactive("")
    show_context_files: reactive[bool] = reactive(True)
    
    def __init__(self, context_mode: str = "project", **kwargs):
        super().__init__(**kwargs)
        self.context_mode = context_mode

    def compose(self) -> ComposeResult:
        """Compose the unified tree widget"""
        with Vertical():
            # Header
            with Vertical(classes="tree-header"):
                yield Static(self._get_header_text(), id="tree_header_text")
            
            # Tree content
            with ScrollableContainer(classes="tree-content"):
                yield self._build_tree_content()
            
            # Controls
            with Vertical(classes="tree-controls"):
                yield Button("🔍 Search", id="search_btn", classes="control-btn")
                yield Button("🔄 Refresh", id="refresh_btn", classes="control-btn")

    def _get_header_text(self) -> str:
        """Get header text based on context mode"""
        if self.context_mode == "project":
            return "📁 Projects"
        elif self.context_mode == "session":
            return "🎯 Sessions"
        elif self.context_mode == "files":
            project_name = self.selected_project or "Files"
            return f"📁 {project_name}"
        else:
            return "📁 Tree"

    def _build_tree_content(self) -> Widget:
        """Build tree content based on context mode"""
        tree = Tree("Root", id="unified_tree")
        tree.show_root = False
        
        if self.context_mode == "project":
            self._build_project_mode(tree)
        elif self.context_mode == "session":
            self._build_session_mode(tree)
        elif self.context_mode == "files":
            self._build_files_mode(tree)
        
        return tree

    def _build_project_mode(self, tree: Tree) -> None:
        """Build project overview mode"""
        projects = get_projects()
        sessions = get_sessions()
        
        for project in projects:
            # Project activity indicators
            status_emoji = self._get_project_status_emoji(project.get('status', 'unknown'))
            activity_level = project.get('activity_level', 0)
            
            # Create project node text
            project_text = Text()
            project_text.append("📁 ")
            project_text.append(f"{project['name']} ", style="bold")
            project_text.append(f"{status_emoji} ")
            if activity_level > 0:
                project_text.append(f"●{activity_level}", style="green")
            
            # Add project node
            project_node = tree.root.add(
                project_text, 
                data={"type": "project", "name": project['name']}
            )
            
            # Add session summary
            project_sessions = [s for s in sessions if s.get('project') == project['name']]
            active_sessions = [s for s in project_sessions if s.get('status') not in ['idle', 'archived']]
            stuck_sessions = [s for s in project_sessions if s.get('status') == 'stuck']
            
            if project_sessions:
                session_text = f"🎯 {len(active_sessions)} active"
                if len(project_sessions) - len(active_sessions) > 0:
                    session_text += f", {len(project_sessions) - len(active_sessions)} idle"
                if stuck_sessions:
                    session_text += f", {len(stuck_sessions)} stuck"
                
                project_node.add_leaf(session_text, data={"type": "session_summary"})
            
            # Add quick actions
            if project_sessions:
                project_node.add_leaf("→ View Sessions", data={"type": "action", "action": "view_sessions", "project": project['name']})
            project_node.add_leaf("→ View Files", data={"type": "action", "action": "view_files", "project": project['name']})

    def _build_session_mode(self, tree: Tree) -> None:
        """Build session management mode"""
        projects = get_projects()
        sessions = get_sessions()
        
        for project in projects:
            project_sessions = [s for s in sessions if s.get('project') == project['name']]
            
            if not project_sessions:
                continue
                
            # Project node
            project_text = Text()
            project_text.append("📁 ")
            project_text.append(f"{project['name']} ", style="bold")
            project_text.append(f"({len(project_sessions)})", style="dim")
            
            project_node = tree.root.add(
                project_text, 
                data={"type": "project", "name": project['name']}
            )
            
            # Session nodes grouped by status
            session_groups = self._group_sessions_by_status(project_sessions)
            
            for status, status_sessions in session_groups.items():
                if not status_sessions:
                    continue
                    
                status_emoji = self._get_session_status_emoji(status)
                status_text = Text()
                status_text.append(f"{status_emoji} ")
                status_text.append(f"{status.title()} ({len(status_sessions)})", style="bold" if status != 'idle' else "dim")
                
                status_node = project_node.add(status_text, data={"type": "status_group", "status": status})
                
                for session in status_sessions:
                    session_text = Text()
                    session_text.append(f"  {session.get('name', session.get('id', 'Unknown'))}")
                    
                    # Add activity indicator
                    if session.get('activity_level', 0) > 0:
                        session_text.append(" ●", style="green")
                    
                    session_node = status_node.add_leaf(
                        session_text, 
                        data={"type": "session", "id": session.get('id'), "project": project['name']}
                    )

    def _build_files_mode(self, tree: Tree) -> None:
        """Build file tree mode for selected project"""
        if not self.selected_project:
            tree.root.add_leaf("Select a project to view files", data={"type": "placeholder"})
            return
            
        files = get_files_by_project(self.selected_project)
        
        # Show context files first if enabled
        if self.show_context_files:
            sessions = get_sessions()
            project_sessions = [s for s in sessions if s.get('project') == self.selected_project]
            
            context_files = []
            for session in project_sessions:
                context_files.extend(get_context_files_by_session(session.get('id')))
            
            if context_files:
                context_text = Text()
                context_text.append("📂 In Context ", style="bold")
                context_text.append(f"({len(context_files)})", style="dim")
                
                context_node = tree.root.add(context_text, data={"type": "context_group"})
                
                for file in context_files:
                    file_text = Text()
                    file_text.append("📄 ")
                    file_text.append(file.get('name', 'Unknown'))
                    
                    # Activity indicator
                    if file.get('activity_level', 0) > 0:
                        file_text.append(" ●", style="green")
                    
                    context_node.add_leaf(
                        file_text, 
                        data={"type": "file", "path": file.get('path'), "project": self.selected_project}
                    )
        
        # Group remaining files by directory
        directories = self._group_files_by_directory(files)
        
        for dir_name, file_list in directories.items():
            if dir_name:
                # Directory node
                dir_text = Text()
                dir_text.append("📁 ")
                dir_text.append(f"{dir_name}/ ", style="bold")
                dir_text.append(f"({len(file_list)})", style="dim")
                
                dir_node = tree.root.add(dir_text, data={"type": "directory", "path": dir_name})
                
                # Files in directory
                for file in file_list:
                    file_text = Text()
                    file_text.append("📄 ")
                    file_text.append(file.get('name', 'Unknown'))
                    
                    # Activity indicator
                    if file.get('activity_level', 0) > 0:
                        file_text.append(" ●", style="green")
                    
                    dir_node.add_leaf(
                        file_text, 
                        data={"type": "file", "path": file.get('path'), "project": self.selected_project}
                    )
            else:
                # Root level files
                for file in file_list:
                    file_text = Text()
                    file_text.append("📄 ")
                    file_text.append(file.get('name', 'Unknown'))
                    
                    if file.get('activity_level', 0) > 0:
                        file_text.append(" ●", style="green")
                    
                    tree.root.add_leaf(
                        file_text, 
                        data={"type": "file", "path": file.get('path'), "project": self.selected_project}
                    )

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
            'idle': '💤',
            'available': '○'
        }
        return status_map.get(status, '❓')

    def _group_sessions_by_status(self, sessions: List[Dict[str, Any]]) -> Dict[str, List[Dict[str, Any]]]:
        """Group sessions by their status"""
        groups = {
            'implementing': [],
            'planning': [],
            'analyzing': [],
            'reviewing': [],
            'stuck': [],
            'idle': []
        }
        
        for session in sessions:
            status = session.get('status', 'idle')
            if status in groups:
                groups[status].append(session)
            else:
                groups['idle'].append(session)
        
        return groups

    def _group_files_by_directory(self, files: List[Dict[str, Any]]) -> Dict[str, List[Dict[str, Any]]]:
        """Group files by their directory"""
        directories = {}
        
        for file in files:
            file_path = file.get('path', '')
            path_parts = Path(file_path).parts
            
            if len(path_parts) > 1:
                dir_name = path_parts[0]
            else:
                dir_name = ""
            
            if dir_name not in directories:
                directories[dir_name] = []
            directories[dir_name].append(file)
        
        return directories

    def set_context_mode(self, mode: str, selected_project: str = "") -> None:
        """Switch context mode and rebuild tree"""
        self.context_mode = mode
        if selected_project:
            self.selected_project = selected_project
        
        # Update header
        header = self.query_one("#tree_header_text", Static)
        header.update(self._get_header_text())
        
        # Rebuild tree
        self._rebuild_tree()

    def set_selected_project(self, project_name: str) -> None:
        """Set selected project and switch to files mode if needed"""
        self.selected_project = project_name
        
        if self.context_mode == "files":
            # Update header
            header = self.query_one("#tree_header_text", Static)
            header.update(self._get_header_text())
            
            # Rebuild tree
            self._rebuild_tree()

    def toggle_context_files(self) -> None:
        """Toggle showing context files separately"""
        self.show_context_files = not self.show_context_files
        
        if self.context_mode == "files":
            self._rebuild_tree()

    def _rebuild_tree(self) -> None:
        """Rebuild the tree content"""
        tree_container = self.query_one(".tree-content", ScrollableContainer)
        tree_container.remove_children()
        tree_container.mount(self._build_tree_content())

    def on_tree_node_selected(self, event: Tree.NodeSelected) -> None:
        """Handle tree node selection"""
        node = event.node
        data = node.data
        
        if not data:
            return
            
        data_type = data.get("type")
        
        if data_type == "project":
            self.post_message(self.ProjectSelected(data["name"]))
        elif data_type == "session":
            self.post_message(self.SessionSelected(data["id"], data["project"]))
        elif data_type == "file":
            self.post_message(self.FileSelected(data["path"], data["project"]))
        elif data_type == "action":
            action = data.get("action")
            if action == "view_sessions":
                self.set_context_mode("session")
            elif action == "view_files":
                self.set_context_mode("files", data["project"])

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle control button presses"""
        button_id = event.button.id
        
        if button_id == "refresh_btn":
            self.post_message(self.RefreshRequested())
            self._rebuild_tree()
        elif button_id == "search_btn":
            # TODO: Implement search functionality
            pass

    def expand_project(self, project_name: str) -> None:
        """Expand a specific project in the tree"""
        tree = self.query_one("#unified_tree", Tree)
        
        for node in tree.root.children:
            if node.data and node.data.get("name") == project_name:
                node.expand()
                break

    def highlight_item(self, item_type: str, item_name: str) -> None:
        """Highlight a specific item in the tree"""
        # TODO: Add visual highlighting implementation
        pass

    def get_selected_node_data(self) -> Optional[Dict[str, Any]]:
        """Get data from currently selected node"""
        tree = self.query_one("#unified_tree", Tree)
        if tree.cursor_node:
            return tree.cursor_node.data
        return None