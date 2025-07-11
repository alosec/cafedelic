#!/usr/bin/env python3
"""
Main Dashboard Screen
Primary delegation interface integrating all components.
"""

from textual.app import ComposeResult
from textual.containers import Horizontal, Vertical, VerticalScroll
from textual.widgets import Header, Footer, Static
from textual.screen import Screen
from textual.message import Message

from ..components.quick_chat import QuickChatWidget
from ..components.session_tabs import SessionTabsWidget  
from ..components.task_feed import TaskFeedWidget
from ..data.mock_data import get_sessions, get_active_sessions


class MainDashboard(Screen):
    """Main dashboard screen for task delegation platform"""
    
    BINDINGS = [
        ("d", "toggle_dark", "Toggle dark mode"),
        ("r", "refresh", "Refresh data"),
        ("q", "quit", "Quit"),
        ("ctrl+n", "new_session", "New session"),
        ("ctrl+t", "new_task", "New task"),
        ("f1", "help", "Help")
    ]
    
    DEFAULT_CSS = """
    MainDashboard {
        layout: vertical;
    }
    
    MainDashboard Header {
        dock: top;
    }
    
    MainDashboard Footer {
        dock: bottom;
    }
    
    MainDashboard .main-container {
        height: 1fr;
        layout: vertical;
    }
    
    MainDashboard .content-container {
        layout: horizontal;
        height: 1fr;
    }
    
    MainDashboard .left-panel {
        width: 1fr;
        min-width: 60;
    }
    
    MainDashboard .right-panel {
        width: 25;
        min-width: 25;
    }
    
    MainDashboard .stats-bar {
        dock: top;
        height: 1;
        background: $panel;
        padding: 0 1;
        color: $text-muted;
    }
    """
    
    def compose(self) -> ComposeResult:
        """Compose the main dashboard"""
        active_sessions = get_active_sessions()
        total_sessions = len(get_sessions())
        
        yield Header()
        
        with Vertical(classes="main-container"):
            # Stats bar
            with Horizontal(classes="stats-bar"):
                yield Static(f"🎯 Cafedelic Task Delegation Platform")
                yield Static(f"Active: {len(active_sessions)} | Total: {total_sessions} | Health: ● Good", id="stats")
            
            # Quick chat delegation widget (always visible at top)
            yield QuickChatWidget()
            
            # Main content area
            with Horizontal(classes="content-container"):
                # Left panel: Session tabs
                with Vertical(classes="left-panel"):
                    yield SessionTabsWidget()
                
                # Right panel: Task feed
                with Vertical(classes="right-panel"):
                    yield TaskFeedWidget()
        
        yield Footer()
    
    def on_mount(self) -> None:
        """Handle screen mount"""
        self.title = "Cafedelic Task Delegation Platform"
        self.sub_title = f"{len(get_active_sessions())} active sessions"
    
    def on_quick_chat_widget_task_delegated(self, message: QuickChatWidget.TaskDelegated) -> None:
        """Handle task delegation from quick chat widget"""
        # In real implementation, this would:
        # 1. Create task in database
        # 2. Send command to Claude Code session via --resume
        # 3. Update UI to show delegation status
        # 4. Refresh task feed
        
        self.notify(f"✅ Task delegated to {message.session_id}: {message.task}")
        
        # Refresh task feed
        task_feed = self.query_one(TaskFeedWidget)
        task_feed._refresh_feed()
    
    def on_session_tabs_widget_command_sent(self, message: SessionTabsWidget.CommandSent) -> None:
        """Handle command sent to session"""
        # In real implementation, this would:
        # 1. Execute: claude --resume {session_id} "{command}"
        # 2. Track command execution in database
        # 3. Update session status
        # 4. Show progress in UI
        
        self.notify(f"📤 Sent {message.command} to session {message.session_id}")
        
        # Update stats
        self._update_stats()
    
    def action_toggle_dark(self) -> None:
        """Toggle between light and dark themes"""
        self.app.theme = (
            "textual-dark" if self.app.theme == "textual-light" else "textual-light"
        )
    
    def action_refresh(self) -> None:
        """Refresh all data"""
        self.notify("🔄 Refreshing data...")
        
        # Refresh task feed
        task_feed = self.query_one(TaskFeedWidget)
        task_feed._refresh_feed()
        
        # Update stats
        self._update_stats()
        
        self.notify("✅ Data refreshed")
    
    def action_new_session(self) -> None:
        """Create new session"""
        # In real implementation, show session creation modal
        self.notify("🆕 New session creation - coming soon!")
    
    def action_new_task(self) -> None:
        """Create new task"""
        # Focus on quick chat input
        quick_chat = self.query_one(QuickChatWidget)
        task_input = quick_chat.query_one("#task_input")
        task_input.focus()
    
    def action_help(self) -> None:
        """Show help"""
        help_text = """
        🎯 Cafedelic Task Delegation Platform
        
        Quick Actions:
        • Type in Quick Delegate box to assign tasks
        • Click session tabs to view details  
        • Use workflow commands: /plan, /analyze, /act
        • Monitor progress in Task Feed
        
        Keyboard Shortcuts:
        • Ctrl+N: New session
        • Ctrl+T: New task (focus input)
        • R: Refresh data
        • D: Toggle dark mode
        • Q: Quit
        """
        self.notify(help_text.strip())
    
    def _update_stats(self) -> None:
        """Update the stats bar"""
        active_sessions = get_active_sessions()
        total_sessions = len(get_sessions())
        
        stats_widget = self.query_one("#stats", Static)
        stats_widget.update(f"Active: {len(active_sessions)} | Total: {total_sessions} | Health: ● Good")
        
        # Update subtitle
        self.sub_title = f"{len(active_sessions)} active sessions"