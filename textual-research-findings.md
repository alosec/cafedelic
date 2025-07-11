# Textual Framework Research: Patterns for Delegation Platform

## Overview
Research conducted July 11, 2025, analyzing open-source Textual projects to extract reusable patterns for the Cafedelic task delegation platform. Focus on IDE-like applications, file management, chat interfaces, terminal integration, and monitoring dashboards.

## Key Findings Summary

### 1. Architecture Patterns

#### Database-First Pattern (Validated)
- **Elia ChatGPT client**: Uses SQLite for conversation persistence
- **Pattern**: Local database as single source of truth for application state
- **Relevance**: Confirms our SQLite-first architecture for session/task management

#### Plugin/Adapter Architecture
- **Harlequin SQL IDE**: Modular database adapters for different systems
- **Pattern**: Core engine + pluggable adapters for different backends
- **Relevance**: Validates our display adapter pattern (terminal/VS Code/web)

#### Real-Time Update Systems
- **textual-system-monitor**: Multi-rate updates (CPU fast, others slower)
- **Pattern**: Differential refresh rates based on data urgency
- **Relevance**: Perfect for delegation platform with task status updates

### 2. File Management & Navigation Patterns

#### File Tree Implementation
```python
# From Textual code_browser.py example
class CodeBrowser(App):
    def compose(self):
        yield Header()
        yield DirectoryTree("./")  # Built-in widget
        yield ScrollableContainer(Static(id="code"))
    
    def on_directory_tree_file_selected(self, event):
        # Real-time file content display
        self.query_one("#code").update(render_code(event.path))
```

#### Advanced File Managers
- **veld-fm**: Multi-panel tileable layout with preview panes
- **browsr**: Pleasant file explorer with keyboard navigation
- **Features**: Archive management, syntax highlighting, cross-platform

#### Key Patterns:
- DirectoryTree widget for hierarchical navigation
- Multi-panel layouts for simultaneous views
- Real-time preview systems
- Context-sensitive keyboard shortcuts

### 3. Chat Interface Patterns

#### Conversation Management
```python
# Pattern from Elia and chatui projects
class ChatInterface:
    def __init__(self):
        self.db = sqlite3.connect("conversations.db")
        self.current_conversation = None
    
    async def send_message(self, content):
        # Stream response with real-time UI updates
        async for chunk in self.llm_client.stream(content):
            self.update_display(chunk)
        
        # Persist to database
        self.save_conversation()
```

#### UI Layout Patterns
- **Elia**: Inline vs full-screen chat modes
- **chatui**: Terminal-optimized conversation flow
- **Pattern**: Scrollable message area + input field + status bar

#### Key Features:
- SQLite conversation persistence
- Streaming response handling
- Model switching capabilities
- Export/import functionality

### 4. Terminal Integration Patterns

#### Session Management
```python
# Pattern from libtmux
import libtmux

class SessionManager:
    def __init__(self):
        self.server = libtmux.Server()
    
    def create_delegation_session(self, project_id, task):
        session = self.server.new_session(
            session_name=f"delegate-{project_id}",
            attach=False
        )
        # Send Claude Code command to session
        session.attached_pane.send_keys(f"claude --project {project_id}")
        return session
```

#### Pane Operations
- **libtmux**: Pythonic tmux wrapper with hierarchical objects
- **Pattern**: Server → Sessions → Windows → Panes
- **Relevance**: Direct integration for our terminal multiplexer support

### 5. Monitoring & Dashboard Patterns

#### Real-Time Status Display
```python
# Pattern from textual-system-monitor
class StatusDashboard(Widget):
    def __init__(self):
        self.set_interval(0.1, self.update_fast)   # CPU updates
        self.set_interval(1.0, self.update_medium) # Memory updates
        self.set_interval(5.0, self.update_slow)   # Disk updates
    
    def update_fast(self):
        self.query_one("#cpu").update(get_cpu_stats())
    
    # Color-coded thresholds
    def format_percentage(self, value):
        if value > 80: return f"[red]{value}%[/red]"
        elif value > 60: return f"[yellow]{value}%[/yellow]"
        else: return f"[green]{value}%[/green]"
```

#### Key Patterns:
- Multi-rate update intervals
- Color-coded status indicators
- Scrollable log panes
- Dynamic threshold displays

### 6. Development Tool Patterns

#### IDE-like Features
- **Harlequin**: Query editor + result display + catalog navigation
- **Django-TUI**: Command inspection + execution interface
- **Pattern**: Multi-pane layout with command palette integration

#### Configuration Management
```toml
# Pattern from Elia config
[models.claude]
name = "Claude 3.5 Sonnet"
provider = "anthropic"
timeout = 30

[theme]
primary = "blue"
secondary = "cyan"
```

### 7. Widget Library Patterns

#### Reusable Components
- **textual-autocomplete**: Dropdown completion widget
- **textual-fspicker**: Filesystem picker widget
- **textual-universal-directorytree**: Remote filesystem support
- **textual-canvas**: Character-based drawing widget

#### Custom Widget Architecture
```python
class DelegationWidget(Widget):
    def compose(self):
        yield Input(placeholder="Describe your task...")
        yield Select(options=self.get_available_sessions())
        yield Button("Delegate", id="delegate")
    
    def on_button_pressed(self, event):
        if event.button.id == "delegate":
            self.delegate_task()
```

## Specific Recommendations for Cafedelic

### 1. Core Architecture Validation
- ✅ **SQLite-first approach** confirmed by Elia's conversation management
- ✅ **Plugin adapter pattern** validated by Harlequin's database adapters
- ✅ **Real-time updates** proven by system monitor differential refresh rates

### 2. Quick-Chat Delegation Interface
```python
# Recommended implementation pattern
class QuickChatWidget(Widget):
    def compose(self):
        yield Input(
            placeholder="Type task to delegate...",
            id="task-input"
        )
        yield Horizontal(
            Select(id="session-select"),  # Available sessions
            Button("Find Similar", id="find-relevant"),
            Button("Delegate", id="delegate")
        )
        yield TaskStatusBar(id="status")
    
    def on_input_submitted(self, event):
        # Auto-suggest relevant sessions
        self.find_relevant_chats(event.value)
```

### 3. Session Management Integration
```python
# tmux integration pattern
class SessionOrchestrator:
    def __init__(self):
        self.tmux = libtmux.Server()
        self.db = sqlite3.connect("cafedelic.db")
    
    async def delegate_task(self, task_description, session_id=None):
        if session_id is None:
            session_id = await self.find_relevant_session(task_description)
        
        session = self.get_tmux_session(session_id)
        session.attached_pane.send_keys(f"claude --resume {session_id}")
        
        # Update database
        self.db.execute(
            "UPDATE sessions SET status='delegated', current_task=? WHERE id=?",
            (task_description, session_id)
        )
```

### 4. Real-Time Task Monitoring
```python
# Multi-rate update pattern
class TaskFeedWidget(Widget):
    def on_mount(self):
        self.set_interval(1.0, self.update_active_tasks)    # Fast updates
        self.set_interval(5.0, self.update_session_status)  # Medium updates
        self.set_interval(30.0, self.update_project_stats)  # Slow updates
    
    async def update_active_tasks(self):
        tasks = await self.db.get_active_tasks()
        for task in tasks:
            status_color = self.get_status_color(task.progress)
            self.query_one(f"#task-{task.id}").update(
                f"[{status_color}]{task.description}[/{status_color}]"
            )
```

### 5. Command Integration Pattern
```python
# ~/.claude/commands/ integration
class CommandDiscovery:
    def scan_project_commands(self, project_path):
        commands_path = Path(project_path) / ".claude" / "commands"
        if commands_path.exists():
            return [f.stem for f in commands_path.glob("*.md")]
        return []
    
    def delegate_command(self, command_name, session_id):
        # Execute project-specific command via delegation
        session = self.get_session(session_id)
        session.send_command(f"/{command_name}")
```

## Implementation Priority

### Phase 1: Core Delegation Interface
1. **QuickChatWidget** - Primary task input with autocomplete
2. **SessionTabsWidget** - Active session management with tmux integration
3. **TaskFeedWidget** - Real-time task status with color-coded progress

### Phase 2: Advanced Intelligence
1. **find_relevant_chats()** - Vector similarity search for session discovery
2. **Project command integration** - Dropdown for ~/.claude/commands/
3. **Real-time activity monitoring** - File operation tracking

### Phase 3: Platform Extensions
1. **Display adapters** - VS Code extension, web dashboard
2. **Advanced orchestration** - Multi-session coordination
3. **Analytics dashboard** - Project-wide intelligence insights

## Key Technical Insights

1. **Textual is production-ready** for complex applications (Harlequin SQL IDE proves this)
2. **SQLite integration patterns** are well-established (Elia demonstrates conversation persistence)
3. **Real-time update systems** scale well with differential refresh rates
4. **tmux integration** is mature and pythonic through libtmux
5. **Plugin architectures** provide excellent extensibility (Harlequin adapters)

## Conclusion

The research validates our V3 architecture decisions and provides concrete implementation patterns from production Textual applications. The delegation platform can leverage proven patterns while innovating in the AI orchestration space.

Most importantly: **Textual is the right choice** for our intelligence layer visualization, with multiple complex applications proving its capabilities for IDE-like functionality, real-time monitoring, and sophisticated terminal interfaces.