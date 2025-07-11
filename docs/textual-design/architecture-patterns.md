# Textual Architecture Patterns for Cafedelic

## Overview

This document details proven architectural patterns from research into 20+ open-source Textual applications, providing specific implementation guidance for Cafedelic's task delegation platform.

## Validated Project Patterns

### 1. SQLite-First Architecture ✅

**Validated by**: Elia ChatGPT client's conversation persistence

```python
# Pattern: Database as single source of truth
class CafedelicDatabase:
    def __init__(self, db_path: str = "~/.cafedelic/intelligence.db"):
        self.db = aiosqlite.connect(db_path)
        self.setup_triggers()
    
    async def setup_triggers(self):
        """Database triggers for real-time UI updates"""
        await self.db.execute("""
            CREATE TRIGGER session_status_change 
            AFTER UPDATE OF status ON sessions
            BEGIN
                INSERT INTO ui_events (type, session_id, data) 
                VALUES ('session_status_changed', NEW.id, NEW.status);
            END
        """)
    
    async def watch_for_changes(self) -> AsyncIterator[UIEvent]:
        """Stream database changes to UI components"""
        async for row in self.db.execute("SELECT * FROM ui_events ORDER BY timestamp"):
            yield UIEvent.from_row(row)
            await self.db.execute("DELETE FROM ui_events WHERE id = ?", [row['id']])
```

### 2. Component-Based UI Architecture ✅

**Validated by**: Harlequin SQL IDE's plugin system

```python
# Pattern: Modular widget composition
class MainDashboard(Widget):
    """Main delegation platform interface"""
    
    def compose(self) -> ComposeResult:
        # Top status bar
        yield StatsBar(id="stats-bar")
        
        # Main content area
        with Horizontal():
            # Left panel: Quick delegation
            with Vertical(classes="left-panel"):
                yield QuickChatWidget(id="quick-chat")
                yield ProjectCommandsWidget(id="project-commands")
            
            # Center panel: Session management
            with Vertical(classes="center-panel"):
                yield SessionTabsWidget(id="session-tabs")
            
            # Right panel: Intelligence feed
            with Vertical(classes="right-panel"):
                yield TaskFeedWidget(id="task-feed")
                yield IntelligenceFeedWidget(id="intelligence-feed")
    
    def on_mount(self):
        """Setup component communication"""
        self.setup_message_routing()
    
    def setup_message_routing(self):
        """Route messages between components"""
        # QuickChat → MainDashboard → TaskFeed
        self.listen(TaskDelegated, self.handle_task_delegated)
        self.listen(SessionStatusChanged, self.handle_session_status_changed)
    
    async def handle_task_delegated(self, message: TaskDelegated):
        """Handle task delegation from QuickChatWidget"""
        # Update task feed
        task_feed = self.query_one(TaskFeedWidget)
        await task_feed.add_task_status(message.task_id, message.session_id)
        
        # Update session tabs
        session_tabs = self.query_one(SessionTabsWidget)
        await session_tabs.update_session_status(message.session_id, "busy")
```

### 3. Real-Time Update Architecture ✅

**Validated by**: Dolphie MySQL monitoring tool's differential updates

```python
# Pattern: Multi-rate update system
class RealTimeUpdater:
    """Manages different update frequencies for different data types"""
    
    def __init__(self, app: CafedelicApp):
        self.app = app
        self.update_intervals = {
            'active_tasks': 1.0,      # 1 second for active task progress
            'session_status': 3.0,    # 3 seconds for session health
            'project_stats': 30.0,    # 30 seconds for project overview
            'database_sync': 5.0      # 5 seconds for database changes
        }
    
    def start_monitoring(self):
        """Start all update intervals"""
        for update_type, interval in self.update_intervals.items():
            self.app.set_interval(interval, getattr(self, f'update_{update_type}'))
    
    async def update_active_tasks(self):
        """High-frequency updates for active task progress"""
        active_tasks = await self.app.db.get_active_tasks()
        task_feed = self.app.query_one(TaskFeedWidget)
        
        for task in active_tasks:
            if task.has_progress_update():
                await task_feed.update_task_progress(task.id, task.progress)
    
    async def update_session_status(self):
        """Medium-frequency updates for session health"""
        sessions = await self.app.db.get_all_sessions()
        session_tabs = self.app.query_one(SessionTabsWidget)
        
        for session in sessions:
            health = await self.get_session_health(session.id)
            if health.status_changed():
                await session_tabs.update_session_health(session.id, health)
```

### 4. Terminal Integration Patterns ✅

**Validated by**: libtmux wrapper for hierarchical session management

```python
# Pattern: Claude Code session lifecycle management
class ClaudeCodeSessionManager:
    """Manages Claude Code instances with tmux integration"""
    
    def __init__(self):
        self.tmux_server = libtmux.Server()
        self.active_sessions = {}
    
    async def create_session(self, project_id: str, config: SessionConfig) -> ClaudeSession:
        """Create new Claude Code session with tmux isolation"""
        # Create tmux session for isolation
        tmux_session = self.tmux_server.new_session(
            session_name=f"claude-{project_id}-{config.task_type}",
            start_directory=config.working_directory,
            detach=True
        )
        
        # Setup pane properties for MCP integration
        pane = tmux_session.attached_pane
        await self.assign_pane_properties(pane, {
            'source': 'claude-code',
            'role': 'ai-assistant',
            'name': f"claude-{config.task_type}",
            'project_id': project_id
        })
        
        # Launch Claude Code in the session
        claude_command = [
            'claude',
            '--project', project_id,
            '--session-name', config.session_name,
            '--working-directory', config.working_directory
        ]
        
        if config.yolo_mode:
            claude_command.append('--yolo')
        
        pane.send_keys(' '.join(claude_command))
        
        # Create session object
        session = ClaudeSession(
            id=config.session_id,
            project_id=project_id,
            tmux_session=tmux_session,
            pane_id=pane.id,
            status='starting',
            config=config
        )
        
        # Start monitoring
        self.active_sessions[session.id] = session
        await self.start_session_monitoring(session)
        
        return session
    
    async def delegate_task(self, session_id: str, task: str) -> TaskResult:
        """Send task to Claude Code session and monitor completion"""
        session = self.active_sessions.get(session_id)
        if not session:
            raise SessionNotFoundError(f"Session {session_id} not found")
        
        if session.status != 'idle':
            raise SessionBusyError(f"Session {session_id} is {session.status}")
        
        # Update session status
        session.status = 'busy'
        session.current_task = task
        await self.update_session_in_database(session)
        
        # Send task to Claude Code
        session.tmux_session.attached_pane.send_keys(task)
        session.tmux_session.attached_pane.send_keys('Enter')
        
        # Monitor for completion
        return await self.monitor_task_completion(session, task)
    
    async def monitor_task_completion(self, session: ClaudeSession, task: str) -> TaskResult:
        """Monitor Claude Code session for task completion"""
        start_time = time.time()
        timeout = 300  # 5 minutes
        
        # Watch log file for completion indicators
        log_path = f"~/.claude/projects/{session.project_id}/transcript.jsonl"
        
        async for log_entry in self.watch_log_file(log_path, start_time):
            # Parse activity from log entry
            activity = self.parse_log_entry(log_entry)
            
            # Store activity in database
            await self.store_activity(session.id, activity)
            
            # Check for completion
            if self.is_task_complete(activity, task):
                session.status = 'idle'
                session.current_task = None
                await self.update_session_in_database(session)
                
                return TaskResult(
                    success=True,
                    session_id=session.id,
                    duration=time.time() - start_time,
                    activities=await self.get_task_activities(session.id, start_time)
                )
            
            # Check for timeout
            if time.time() - start_time > timeout:
                session.status = 'timeout'
                await self.update_session_in_database(session)
                
                return TaskResult(
                    success=False,
                    error="Task timeout",
                    session_id=session.id,
                    duration=timeout
                )
```

### 5. File Tree Integration ✅

**Validated by**: browsr and veld-fm file management patterns

```python
# Pattern: Intelligent file tree with delegation integration
class ProjectFileTree(DirectoryTree):
    """File tree with Claude Code delegation capabilities"""
    
    def __init__(self, project_path: str, **kwargs):
        super().__init__(project_path, **kwargs)
        self.project_path = project_path
        self.delegation_enabled = True
    
    def on_directory_tree_file_selected(self, event: DirectoryTree.FileSelected):
        """Handle file selection for potential delegation"""
        if not self.delegation_enabled:
            return
        
        file_path = event.path
        self.show_file_actions(file_path)
    
    def show_file_actions(self, file_path: Path):
        """Show available actions for selected file"""
        actions = [
            ("Read and analyze", self.delegate_file_analysis),
            ("Refactor code", self.delegate_refactoring),
            ("Add tests", self.delegate_test_creation),
            ("Add documentation", self.delegate_documentation),
            ("Debug issues", self.delegate_debugging)
        ]
        
        # Show action menu
        action_menu = FileActionMenu(file_path, actions)
        self.app.push_screen(action_menu)
    
    async def delegate_file_analysis(self, file_path: Path):
        """Delegate file analysis to appropriate Claude Code session"""
        # Find or create suitable session
        session = await self.find_suitable_session('analysis', file_path)
        
        # Create delegation task
        task = f"Please analyze the file {file_path}. Provide insights on:\n" \
               f"- Code structure and patterns\n" \
               f"- Potential improvements\n" \
               f"- Dependencies and relationships\n" \
               f"- Any issues or concerns"
        
        # Delegate to session
        session_manager = self.app.session_manager
        result = await session_manager.delegate_task(session.id, task)
        
        # Show result in UI
        self.app.show_delegation_result(result)
    
    async def find_suitable_session(self, task_type: str, file_path: Path) -> ClaudeSession:
        """Find most suitable session for file task"""
        # Get sessions working on related files
        related_sessions = await self.app.db.find_sessions_by_file_context(file_path)
        
        # Find idle session or create new one
        for session in related_sessions:
            if session.status == 'idle':
                return session
        
        # Create new session if none available
        return await self.app.session_manager.create_session(
            project_id=self.app.current_project.id,
            config=SessionConfig(
                task_type=task_type,
                working_directory=self.project_path,
                session_name=f"{task_type}-{file_path.stem}"
            )
        )

class FileActionMenu(ModalScreen):
    """Modal for file action selection"""
    
    def __init__(self, file_path: Path, actions: List[Tuple[str, Callable]]):
        super().__init__()
        self.file_path = file_path
        self.actions = actions
    
    def compose(self) -> ComposeResult:
        with Container(classes="file-action-modal"):
            yield Static(f"Actions for: {self.file_path.name}", classes="modal-title")
            
            with Vertical(classes="action-list"):
                for i, (action_name, action_func) in enumerate(self.actions):
                    yield Button(
                        action_name,
                        classes="action-button",
                        id=f"action-{i}"
                    )
            
            with Horizontal(classes="modal-buttons"):
                yield Button("Cancel", variant="default", id="cancel")
    
    def on_button_pressed(self, event: Button.Pressed):
        if event.button.id == "cancel":
            self.dismiss()
        elif event.button.id.startswith("action-"):
            action_index = int(event.button.id.split("-")[1])
            action_func = self.actions[action_index][1]
            self.dismiss()
            self.app.call_later(action_func, self.file_path)
```

### 6. Chat Interface Patterns ✅

**Validated by**: Elia ChatGPT client and chatui implementations

```python
# Pattern: Real-time chat with delegation capabilities
class DelegationChatWidget(Widget):
    """Chat interface for task delegation with session targeting"""
    
    def compose(self) -> ComposeResult:
        yield Static("Task Delegation", classes="chat-header")
        
        # Chat history
        yield RichLog(
            id="chat-history",
            classes="chat-log",
            auto_scroll=True,
            wrap=True
        )
        
        # Session selector
        with Horizontal(classes="session-selector"):
            yield Static("Target Session:", classes="selector-label")
            yield Select(
                options=self.get_session_options(),
                id="session-select",
                classes="session-dropdown"
            )
            yield Button("Find Similar", id="find-similar", classes="find-button")
        
        # Input area
        with Horizontal(classes="input-area"):
            yield TextArea(
                placeholder="Describe the task you want to delegate...",
                id="task-input",
                classes="task-textarea"
            )
            yield Button("Delegate", variant="primary", id="delegate-btn")
    
    def on_mount(self):
        """Setup chat interface"""
        self.update_session_options()
        self.setup_auto_suggestions()
    
    def get_session_options(self) -> List[Tuple[str, str]]:
        """Get available sessions for delegation"""
        sessions = self.app.session_manager.get_available_sessions()
        return [
            (f"{session.name} ({session.status})", session.id)
            for session in sessions
        ]
    
    def setup_auto_suggestions(self):
        """Setup automatic session suggestions based on task content"""
        task_input = self.query_one("#task-input", TextArea)
        task_input.watch("text", self.on_task_text_changed)
    
    async def on_task_text_changed(self, text: str):
        """Suggest sessions based on task content"""
        if len(text) > 10:  # Only suggest after meaningful input
            suggestions = await self.find_relevant_sessions(text)
            await self.update_session_suggestions(suggestions)
    
    async def find_relevant_sessions(self, task_text: str) -> List[SessionSuggestion]:
        """Find sessions relevant to the task using semantic search"""
        # Use find_relevant_chats() implementation
        relevant_chats = await self.app.db.find_relevant_chats(
            query=task_text,
            limit=5,
            include_context=True
        )
        
        suggestions = []
        for chat in relevant_chats:
            session = await self.app.db.get_session(chat.session_id)
            if session and session.status in ['idle', 'available']:
                suggestions.append(SessionSuggestion(
                    session=session,
                    relevance_score=chat.similarity_score,
                    context_snippet=chat.context_snippet
                ))
        
        return suggestions
    
    async def on_button_pressed(self, event: Button.Pressed):
        """Handle button interactions"""
        if event.button.id == "delegate-btn":
            await self.handle_task_delegation()
        elif event.button.id == "find-similar":
            await self.handle_find_similar()
    
    async def handle_task_delegation(self):
        """Handle task delegation to selected session"""
        task_input = self.query_one("#task-input", TextArea)
        session_select = self.query_one("#session-select", Select)
        chat_log = self.query_one("#chat-history", RichLog)
        
        task_text = task_input.text.strip()
        selected_session_id = session_select.value
        
        if not task_text:
            chat_log.write("[red]Error:[/] Please enter a task description")
            return
        
        if not selected_session_id:
            chat_log.write("[red]Error:[/] Please select a target session")
            return
        
        # Show delegation in progress
        chat_log.write(f"[bold blue]Delegating:[/] {task_text}")
        chat_log.write(f"[dim]Target Session:[/] {selected_session_id}")
        
        try:
            # Delegate task
            result = await self.app.session_manager.delegate_task(
                session_id=selected_session_id,
                task=task_text
            )
            
            # Show result
            if result.success:
                chat_log.write(f"[green]✓ Task delegated successfully[/]")
                chat_log.write(f"[dim]Task ID:[/] {result.task_id}")
                
                # Clear input
                task_input.text = ""
                
                # Post message for other components
                self.post_message(TaskDelegated(
                    task_id=result.task_id,
                    session_id=selected_session_id,
                    task_text=task_text
                ))
            else:
                chat_log.write(f"[red]✗ Delegation failed:[/] {result.error}")
        
        except Exception as e:
            chat_log.write(f"[red]Error:[/] {str(e)}")
    
    async def handle_find_similar(self):
        """Handle finding similar conversations"""
        task_input = self.query_one("#task-input", TextArea)
        chat_log = self.query_one("#chat-history", RichLog)
        
        task_text = task_input.text.strip()
        if not task_text:
            chat_log.write("[yellow]Enter a task description to find similar conversations[/]")
            return
        
        # Find similar conversations
        similar_chats = await self.find_relevant_sessions(task_text)
        
        if similar_chats:
            chat_log.write(f"[cyan]Found {len(similar_chats)} similar conversations:[/]")
            for chat in similar_chats[:3]:  # Show top 3
                chat_log.write(
                    f"[dim]• {chat.session.name}[/] "
                    f"(similarity: {chat.relevance_score:.1%})\n"
                    f"  {chat.context_snippet}"
                )
        else:
            chat_log.write("[yellow]No similar conversations found[/]")

# Message types for component communication
class TaskDelegated(Message):
    def __init__(self, task_id: str, session_id: str, task_text: str):
        super().__init__()
        self.task_id = task_id
        self.session_id = session_id
        self.task_text = task_text

class SessionSuggestion:
    def __init__(self, session: ClaudeSession, relevance_score: float, context_snippet: str):
        self.session = session
        self.relevance_score = relevance_score
        self.context_snippet = context_snippet
```

### 7. Performance Optimization Patterns ✅

**Validated by**: System monitoring tools with high-frequency updates

```python
# Pattern: Efficient database polling and caching
class PerformanceOptimizedUpdater:
    """Optimized update system for real-time UI responsiveness"""
    
    def __init__(self, app: CafedelicApp):
        self.app = app
        self.cache = {}
        self.cache_ttl = {}
        self.update_queue = asyncio.Queue()
    
    @lru_cache(maxsize=128)
    def get_session_summary(self, session_id: str, timestamp: int) -> str:
        """Cached session summaries with timestamp-based invalidation"""
        # timestamp parameter ensures cache invalidation
        return self.app.db.get_session_summary_sync(session_id)
    
    async def efficient_database_sync(self):
        """Efficient database synchronization with minimal queries"""
        # Single query for all session status updates
        updates = await self.app.db.execute("""
            SELECT s.id, s.status, s.last_activity, 
                   COUNT(t.id) as active_tasks,
                   MAX(t.updated_at) as latest_task_update
            FROM sessions s
            LEFT JOIN tasks t ON s.id = t.session_id AND t.status = 'active'
            WHERE s.updated_at > ?
            GROUP BY s.id
        """, [self.get_last_sync_time()])
        
        # Batch update UI components
        if updates:
            await self.batch_update_session_widgets(updates)
    
    async def batch_update_session_widgets(self, updates: List[dict]):
        """Batch update multiple widgets efficiently"""
        session_tabs = self.app.query_one(SessionTabsWidget)
        task_feed = self.app.query_one(TaskFeedWidget)
        
        # Group updates by widget
        session_updates = []
        task_updates = []
        
        for update in updates:
            session_updates.append({
                'session_id': update['id'],
                'status': update['status'],
                'last_activity': update['last_activity']
            })
            
            if update['active_tasks'] > 0:
                task_updates.append({
                    'session_id': update['id'],
                    'task_count': update['active_tasks'],
                    'latest_update': update['latest_task_update']
                })
        
        # Single batch update per widget
        await session_tabs.batch_update_sessions(session_updates)
        await task_feed.batch_update_tasks(task_updates)
    
    def invalidate_cache(self, cache_key: str = None):
        """Intelligent cache invalidation"""
        if cache_key:
            self.cache.pop(cache_key, None)
            self.cache_ttl.pop(cache_key, None)
        else:
            # Clear expired entries
            current_time = time.time()
            expired_keys = [
                key for key, ttl in self.cache_ttl.items()
                if current_time > ttl
            ]
            for key in expired_keys:
                self.cache.pop(key, None)
                self.cache_ttl.pop(key, None)
```

## Component Communication Architecture

```python
# Pattern: Message-based component communication
class ComponentMessageBus:
    """Central message routing for component communication"""
    
    def __init__(self, app: CafedelicApp):
        self.app = app
        self.message_handlers = defaultdict(list)
    
    def register_handler(self, message_type: Type[Message], handler: Callable):
        """Register message handler"""
        self.message_handlers[message_type].append(handler)
    
    async def route_message(self, message: Message):
        """Route message to all registered handlers"""
        handlers = self.message_handlers.get(type(message), [])
        await asyncio.gather(*[handler(message) for handler in handlers])
    
    def setup_delegation_routing(self):
        """Setup routing for delegation platform messages"""
        # Task delegation flow
        self.register_handler(TaskDelegated, self.handle_task_delegated)
        self.register_handler(TaskCompleted, self.handle_task_completed)
        self.register_handler(SessionStatusChanged, self.handle_session_status_changed)
        
        # Intelligence updates
        self.register_handler(ActivityDetected, self.handle_activity_detected)
        self.register_handler(ContextUpdated, self.handle_context_updated)
    
    async def handle_task_delegated(self, message: TaskDelegated):
        """Handle task delegation across components"""
        # Update task feed
        task_feed = self.app.query_one(TaskFeedWidget)
        await task_feed.add_task_entry(message.task_id, message.session_id)
        
        # Update session status
        session_tabs = self.app.query_one(SessionTabsWidget)
        await session_tabs.mark_session_busy(message.session_id)
        
        # Update statistics
        stats_bar = self.app.query_one(StatsBar)
        await stats_bar.increment_active_tasks()
```

These patterns provide the architectural foundation for implementing Cafedelic's delegation platform with proven, production-ready approaches validated by existing Textual applications.