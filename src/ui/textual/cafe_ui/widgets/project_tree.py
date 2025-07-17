#!/usr/bin/env python3
"""
ProjectTree Widget
Unified DirectoryTree widget displaying projects, sessions, and files in hierarchy.
"""

from textual.widgets import DirectoryTree
from textual.app import ComposeResult
from textual.message import Message
from textual.reactive import reactive
from textual.widget import Widget
from textual.containers import Vertical
from textual.widgets import Static, Button
from rich.text import Text
from pathlib import Path
from typing import Optional

from ..data.claude_code_data import (
    get_projects, get_sessions, get_files_by_project, 
    get_context_files_by_session, get_activity_emoji
)


class ProjectTree(Widget):
    """Unified project tree with projects, sessions, and files"""
    
    DEFAULT_CSS = """
    ProjectTree {
        width: 1fr;
        height: 1fr;
        border: solid $primary;
        padding: 1;
    }
    
    ProjectTree .project-node {
        margin: 0 0 1 0;
    }
    
    ProjectTree .session-tree {
        margin: 0 0 0 2;
    }
    
    ProjectTree .file-tree {
        margin: 0 0 0 2;
    }
    
    ProjectTree .tree-item {
        padding: 0;
        margin: 0;
    }
    
    ProjectTree .tree-item:hover {
        background: $primary 20%;
    }
    
    ProjectTree .open-chat-btn {
        background: $accent;
        color: $text;
        margin: 0 1;
        padding: 0 1;
        border: none;
        min-width: 0;
    }
    
    ProjectTree .open-chat-btn:hover {
        background: $accent 80%;
    }
    """

    class ProjectSelected(Message):
        """Message sent when a project is selected"""
        def __init__(self, project_name: str) -> None:
            self.project_name = project_name
            super().__init__()

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

    expanded_projects: reactive[set] = reactive(set)

    def compose(self) -> ComposeResult:
        """Compose the project tree"""
        projects = get_projects()
        sessions = get_sessions()
        
        for project in projects:
            # Project node
            project_activity = get_activity_emoji(project.activity_level)
            project_status_emoji = self._get_project_status_emoji(project.status)
            
            project_text = Text()
            project_text.append("📁 ")
            project_text.append(f"{project.name} ", style="bold")
            project_text.append(f"{project_activity} {project_status_emoji}")
            
            project_node = Button(
                project_text,
                id=f"project_{project.name}",
                classes="tree-item"
            )
            project_node.can_focus = True
            yield project_node
            
            # Session tree for this project
            project_sessions = [s for s in sessions if s.project == project.name]
            if project_sessions:
                # Session tree header
                yield Static("├─ 🎯 SessionTree")
                
                for session in project_sessions:
                    # Session status line
                    status_emoji = self._get_session_status_emoji(session.status)
                    session_text = f"│  ├─ {status_emoji} {session.name}"
                    session_node = Button(
                        session_text,
                        id=f"session_{session.id}",
                        classes="tree-item"
                    )
                    yield session_node
                    
                    # Open chat button
                    chat_btn_text = "│  │  └─ [Open Chat]"
                    chat_btn = Button(
                        chat_btn_text,
                        id=f"chat_{session.id}",
                        classes="open-chat-btn tree-item"
                    )
                    yield chat_btn
            
            # File tree for this project
            yield Static("└─ 📁 FileTree")
            
            # In Context files
            context_files = []
            for session in project_sessions:
                context_files.extend(get_context_files_by_session(session.id))
            
            if context_files:
                yield Static("   ├─ 📂 In Context")
                
                for file in context_files:
                    file_activity = get_activity_emoji(file.activity_level)
                    file_text = f"   │  ├─ 📄 {file.name} {file_activity}"
                    yield Static(file_text, classes="tree-item")
            
            # All Files
            all_files = get_files_by_project(project.name)
            if all_files:
                yield Static("   └─ 📂 All Files")
                
                # Group files by directory
                directories = self._group_files_by_directory(all_files)
                for dir_name, files in directories.items():
                    if dir_name:
                        dir_text = f"      ├─ 📁 {dir_name}/"
                        yield Static(dir_text, classes="tree-item")
                        
                        for file in files:
                            file_text = f"      │  └─ 📄 {file.name}"
                            yield Static(file_text, classes="tree-item")
                    else:
                        # Root level files
                        for file in files:
                            file_text = f"      └─ 📄 {file.name}"
                            yield Static(file_text, classes="tree-item")
            
            # Add spacing between projects
            yield Static("")


    def _get_project_status_emoji(self, status: str) -> str:
        """Get emoji for project status"""
        status_map = {
            'active': '🔥',
            'idle': '💤',
            'issues': '⚠️'
        }
        return status_map.get(status, '❓')

    def _get_session_status_emoji(self, status: str) -> str:
        """Get emoji for session status"""
        status_map = {
            'planning': '●',
            'analyzing': '○',
            'implementing': '●',
            'reviewing': '○',
            'stuck': '⚠',
            'available': '○'
        }
        return status_map.get(status, '❓')

    def _group_files_by_directory(self, files: list[MockFile]) -> dict[str, list[MockFile]]:
        """Group files by their directory"""
        directories = {}
        
        for file in files:
            path_parts = Path(file.path).parts
            if len(path_parts) > 1:
                dir_name = path_parts[0]
            else:
                dir_name = ""
            
            if dir_name not in directories:
                directories[dir_name] = []
            directories[dir_name].append(file)
        
        return directories

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Handle button presses in the tree"""
        button_id = event.button.id
        
        if button_id and button_id.startswith("project_"):
            project_name = button_id.replace("project_", "")
            self.post_message(self.ProjectSelected(project_name))
        
        elif button_id and button_id.startswith("session_"):
            session_id = button_id.replace("session_", "")
            self.post_message(self.SessionSelected(session_id))
        
        elif button_id and button_id.startswith("chat_"):
            session_id = button_id.replace("chat_", "")
            self.post_message(self.ChatRequested(session_id))

    def refresh_tree(self) -> None:
        """Refresh the entire tree"""
        # Remove all children and rebuild
        self.remove_children()
        # Note: In real implementation, would need to trigger recompose