# Textual UI Design Guidelines for Cafedelic Delegation Platform

## Overview

This document synthesizes comprehensive research findings on Textual framework capabilities, existing project patterns, Claude Code integration approaches, and modern terminal UI design principles to provide actionable implementation guidance for Cafedelic's task delegation platform.

## Framework Validation

### Textual Framework Assessment ✅

**Perfect Architectural Match**: Textual's component-based, reactive, database-driven design aligns exceptionally well with Cafedelic's V3 intelligence layer vision.

**Production Readiness Confirmed**:
- **Harlequin SQL IDE**: Demonstrates sophisticated IDE functionality with plugin architecture
- **Multiple file managers**: Prove robust filesystem interaction (veld-fm, browsr)
- **Chat applications**: Show real-time streaming capabilities (Elia, chatui)
- **Monitoring tools**: Validate high-frequency update patterns (Dolphie, system monitors)

**Complete Widget Ecosystem Available**:
- `TabbedContent` for session management
- `DirectoryTree` for file browsing
- `RichLog` for chat interfaces
- `DataTable` for activity displays
- `ProgressBar` for task status
- `Input/TextArea` for command delegation

## Core Design Principles

### 1. Intelligence-First Architecture

**Separation of Concerns**:
- **Intelligence Layer** (Cafedelic core): SQLite database, context reasoning, session orchestration
- **Display Layer** (Textual UI): Component presentation, user interaction, visual feedback
- **Communication Layer** (MCP/Async): Claude Code integration, real-time updates

**Database-Driven UI**:
```python
# Reactive attributes mirroring SQLite state
class SessionTabsWidget(Widget):
    sessions = reactive(list)  # Auto-updates from database changes
    active_session = reactive(str)
    
    def watch_sessions(self, sessions):
        """Automatically re-render when sessions change"""
        self.update_tabs(sessions)
```

### 2. Task Delegation Interface Patterns

**Quick-Chat Widget Design**:
```python
class QuickChatWidget(Widget):
    def compose(self):
        yield Static("Task Delegation", classes="header")
        yield Input(
            placeholder="Type task to delegate...",
            id="task-input",
            classes="delegation-input"
        )
        yield Horizontal(
            Select(options=self.available_sessions, id="session-select"),
            Button("Find Similar", id="find-similar"),
            Button("Delegate", variant="primary", id="delegate-btn")
        )
        yield RichLog(id="delegation-status", classes="status-feed")
```

**Session Targeting Strategy**:
- Dropdown selection of available sessions
- Context-aware session suggestions based on task content
- `find_relevant_chats()` integration for discovering similar previous work
- Visual session status indicators (Planning/Analyzing/Implementing/Stuck)

### 3. Real-Time Status Visualization

**Multi-Rate Update Strategy**:
```python
# Different update frequencies for different data types
class TaskFeedWidget(Widget):
    def on_mount(self):
        # Active tasks need frequent updates
        self.set_interval(1.0, self.update_active_tasks)
        # Session status updates less frequently
        self.set_interval(5.0, self.update_session_status)
        # Project stats update rarely
        self.set_interval(30.0, self.update_project_stats)
```

**Progress Visualization Patterns**:
```
[Task Description] [45%] [████████░░] [Current/Total] [Rate] [ETA]
Quick-Chat: [Implement auth] → [auth-session] | Status: Planning... | Progress: [████░░░░] 40%
```

### 4. Professional Theming System

**TCSS Stylesheets** (Textual CSS):
```css
/* delegation-theme.tcss */
.delegation-input {
    border: solid $primary;
    background: $panel;
    padding: 1;
}

.status-feed {
    border: solid $accent;
    height: 1fr;
    scrollbar-gutter: stable;
}

SessionTabsWidget Tab {
    background: $surface;
    color: $text;
}

SessionTabsWidget Tab:hover {
    background: $primary;
}

.task-planning { background: $warning; }
.task-analyzing { background: $accent; }
.task-implementing { background: $primary; }
.task-stuck { background: $error; }
```

## Implementation Architecture

### 1. Component Hierarchy

```
MainDashboard
├── StatsBar (project health, active sessions count)
├── QuickChatWidget
│   ├── Input (task delegation)
│   ├── SessionSelect (target session)
│   └── ActionButtons (Find Similar, Delegate)
├── TabbedContent
│   ├── SessionTabsWidget
│   │   └── SessionDetailPane (per session)
│   ├── ProjectFileTree
│   └── IntelligenceFeed
└── TaskFeedWidget (real-time delegation status)
```

### 2. Database Integration Patterns

**Reactive Data Binding**:
```python
class CafedelicApp(App):
    def __init__(self):
        super().__init__()
        self.db = CafedelicDatabase()
        self.setup_watchers()
    
    def setup_watchers(self):
        # Watch for database changes
        self.set_interval(2.0, self.sync_database_state)
    
    async def sync_database_state(self):
        # Query latest state from SQLite
        sessions = await self.db.get_active_sessions()
        tasks = await self.db.get_recent_tasks()
        
        # Update reactive attributes
        self.query_one(SessionTabsWidget).sessions = sessions
        self.query_one(TaskFeedWidget).tasks = tasks
```

### 3. Claude Code Integration Layer

**Async Communication Patterns**:
```python
class ClaudeCodeIntegration:
    async def delegate_task(self, session_id: str, task: str) -> TaskResult:
        # Send task to Claude Code instance
        process = await self.get_session_process(session_id)
        await process.send_input(task)
        
        # Monitor for completion with real-time updates
        async for update in self.monitor_session_activity(session_id):
            # Update UI in real-time
            self.app.notify_task_progress(session_id, update)
            
            if update.type == 'completion':
                return TaskResult(success=True, output=update.result)
        
        return TaskResult(success=False, error="Task timeout")
```

**Log Monitoring Integration**:
```python
async def monitor_claude_code_logs(session_id: str):
    """Real-time Claude Code activity monitoring"""
    log_path = f"~/.claude/projects/{session_id}/transcript.jsonl"
    
    async for entry in watch_jsonl_file(log_path):
        activity = parse_log_entry(entry)
        await store_activity_in_database(session_id, activity)
        
        # Trigger UI updates via message passing
        app.post_message(ActivityUpdated(session_id, activity))
```

## Proven Implementation Patterns

### 1. File Tree Integration

**Based on browsr and veld-fm patterns**:
```python
class ProjectFileTree(DirectoryTree):
    def on_directory_tree_file_selected(self, event):
        """Handle file selection for Claude Code sessions"""
        file_path = event.path
        active_session = self.app.get_active_session()
        
        if active_session:
            # Send file to active Claude Code session
            self.app.delegate_file_task(active_session.id, file_path)
```

### 2. Terminal Integration Patterns

**Based on libtmux wrapper patterns**:
```python
class SessionManager:
    def __init__(self):
        self.tmux_server = libtmux.Server()
    
    async def create_claude_session(self, project_id: str) -> Session:
        # Create isolated tmux session for Claude Code
        session = self.tmux_server.new_session(
            session_name=f"claude-{project_id}",
            start_directory=self.get_project_path(project_id)
        )
        
        # Launch Claude Code in the session
        session.attached_pane.send_keys(f"claude --project {project_id}")
        
        return Session(
            id=session.id,
            tmux_session=session,
            project_id=project_id,
            status='active'
        )
```

### 3. Chat Interface Patterns

**Based on Elia ChatGPT client**:
```python
class DelegationChatWidget(Widget):
    def compose(self):
        yield RichLog(classes="chat-history", id="history")
        yield Horizontal(
            Input(placeholder="Delegate task...", id="input"),
            Button("Send", variant="primary", id="send")
        )
    
    def on_button_pressed(self, event):
        if event.button.id == "send":
            task_text = self.query_one("#input").value
            self.delegate_task(task_text)
    
    async def delegate_task(self, task: str):
        # Show immediate feedback
        self.query_one("#history").write(f"[bold blue]Delegating:[/] {task}")
        
        # Find appropriate session
        session = await self.find_relevant_session(task)
        
        # Execute delegation
        result = await self.claude_integration.delegate_task(session.id, task)
        
        # Show result
        status_color = "green" if result.success else "red"
        self.query_one("#history").write(f"[{status_color}]Result:[/] {result.message}")
```

## Performance Guidelines

### 1. Database Query Optimization

**Target: <50ms query response times**:
```python
# Efficient session lookup with indexes
async def find_relevant_sessions(self, task_content: str) -> List[Session]:
    # Use full-text search on session context
    query = """
        SELECT s.*, sc.summary 
        FROM sessions s 
        JOIN session_context sc ON s.id = sc.session_id
        WHERE sc.summary_fts MATCH ?
        ORDER BY sc.relevance_score DESC
        LIMIT 5
    """
    return await self.db.execute(query, [task_content])
```

### 2. Memory-Efficient Updates

**Lazy loading and caching**:
```python
from functools import lru_cache

class IntelligenceFeed(Widget):
    @lru_cache(maxsize=100)
    def get_session_summary(self, session_id: str) -> str:
        """Cached session summaries to avoid repeated queries"""
        return self.db.get_session_summary(session_id)
    
    def on_unmount(self):
        # Clear cache when widget unmounts
        self.get_session_summary.cache_clear()
```

### 3. Responsive UI Updates

**Progressive enhancement pattern**:
```python
class TaskProgressWidget(Widget):
    def update_progress(self, task_id: str, progress: float):
        # Immediate local update for responsiveness
        progress_bar = self.query_one(f"#progress-{task_id}")
        progress_bar.progress = progress
        
        # Background database update
        self.run_worker(self.store_progress(task_id, progress))
    
    async def store_progress(self, task_id: str, progress: float):
        """Background worker to persist progress"""
        await self.db.update_task_progress(task_id, progress)
```

## Keyboard Navigation & Accessibility

### 1. Modal Editing Patterns

**Helix-inspired selection-first editing**:
```python
class DelegationInterface(Widget):
    BINDINGS = [
        ("tab", "next_field", "Next field"),
        ("shift+tab", "previous_field", "Previous field"),
        ("enter", "confirm_action", "Confirm"),
        ("escape", "cancel_action", "Cancel"),
        ("ctrl+c", "interrupt", "Interrupt"),
        ("ctrl+f", "find_similar", "Find similar"),
        ("ctrl+d", "delegate", "Delegate task"),
    ]
    
    def action_find_similar(self):
        """Find similar conversations for current task"""
        task_text = self.query_one("#task-input").value
        similar_sessions = self.find_relevant_chats(task_text)
        self.show_similar_sessions(similar_sessions)
```

### 2. Screen Reader Compatibility

**Semantic markup and announcements**:
```python
class AccessibleTaskStatus(Widget):
    def update_status(self, session_id: str, status: str):
        # Visual update
        status_widget = self.query_one(f"#status-{session_id}")
        status_widget.update(status)
        
        # Accessibility announcement
        self.app.bell()  # Audio feedback
        self.app.notify(f"Session {session_id} status: {status}")
```

## Integration Testing Patterns

### 1. Mock Data System

**Comprehensive testing environment**:
```python
# Based on existing mock_data.py
class TestingEnvironment:
    def create_mock_sessions(self) -> List[MockSession]:
        return [
            MockSession("auth-session", "Planning", "Implementing authentication system"),
            MockSession("ui-session", "Implementing", "Building delegation interface"),
            MockSession("db-session", "Stuck", "Database migration issues")
        ]
    
    def simulate_claude_code_activity(self, session_id: str):
        """Generate realistic activity patterns for testing"""
        activities = [
            MockActivity("file_read", "src/auth/login.py"),
            MockActivity("thinking", "Analyzing authentication flow"),
            MockActivity("file_write", "src/auth/auth_service.py")
        ]
        return activities
```

### 2. Component Testing

**Isolated widget testing**:
```python
async def test_quick_chat_delegation():
    app = CafedelicApp()
    async with app.run_test() as pilot:
        # Type task into delegation input
        await pilot.click("#task-input")
        await pilot.type("Implement user authentication")
        
        # Select target session
        await pilot.click("#session-select")
        await pilot.click(".option[data-value='auth-session']")
        
        # Delegate task
        await pilot.click("#delegate-btn")
        
        # Verify task was delegated
        status_log = app.query_one("#delegation-status")
        assert "Delegating to auth-session" in status_log.renderable
```

## Next Steps & Implementation Priority

### Phase 1: Core Components (Current Sprint)
1. **Implement QuickChatWidget** with session targeting and delegation logic
2. **Build SessionTabsWidget** with task-state visualization
3. **Create TaskFeedWidget** for real-time status updates
4. **Integrate MainDashboard** with component communication

### Phase 2: Intelligence Integration
1. **Connect to V3 SQLite database** with reactive data binding
2. **Implement find_relevant_chats()** function with semantic search
3. **Add Claude Code process management** and log monitoring
4. **Create MCP tool integration** for delegation commands

### Phase 3: Advanced Features
1. **Professional theming system** with TCSS stylesheets
2. **Advanced keyboard navigation** and accessibility features
3. **Performance optimization** and caching strategies
4. **Testing framework** and automated validation

This comprehensive research validates that Textual is the optimal choice for Cafedelic's delegation platform UI, providing both the technical capabilities and proven patterns needed for a sophisticated task delegation interface.