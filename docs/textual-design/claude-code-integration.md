# Claude Code Integration Patterns for Cafedelic

## Overview

This document provides comprehensive technical approaches for integrating Cafedelic's delegation platform with Claude Code programmatically, based on research into official SDK capabilities, community projects, and proven integration patterns.

## Official Claude Code SDK Architecture

### Multi-Language SDK Support

**TypeScript SDK (Primary)**:
```typescript
import { query, type SDKMessage } from "@anthropic-ai/claude-code";

// Async session management with streaming
class ClaudeCodeSDKManager {
    async createSession(config: SessionConfig): Promise<SessionManager> {
        const messages: SDKMessage[] = [];
        
        const sessionGenerator = query({
            prompt: config.initialPrompt,
            options: {
                maxTurns: config.maxTurns || 50,
                sessionId: config.sessionId,
                workingDirectory: config.workingDirectory,
                tools: config.allowedTools,
                yolo: config.yoloMode || false
            }
        });
        
        return new SessionManager(sessionGenerator, config);
    }
    
    async delegateTask(sessionId: string, task: string): Promise<TaskResult> {
        const session = this.activeSessions.get(sessionId);
        if (!session) {
            throw new SessionNotFoundError(`Session ${sessionId} not found`);
        }
        
        // Send task and collect response
        const response = await session.sendMessage(task);
        return this.processResponse(response);
    }
}
```

**Python SDK (Alternative)**:
```python
import asyncio
from anthropic_claude_code import ClaudeCodeClient

class PythonSDKIntegration:
    def __init__(self):
        self.client = ClaudeCodeClient()
        self.active_sessions = {}
    
    async def create_session(self, config: SessionConfig) -> ClaudeSession:
        """Create new Claude Code session via Python SDK"""
        session = await self.client.create_session(
            project_path=config.working_directory,
            session_name=config.session_name,
            tools=config.allowed_tools,
            max_turns=config.max_turns
        )
        
        self.active_sessions[session.id] = session
        return session
    
    async def delegate_task_async(self, session_id: str, task: str) -> AsyncIterator[TaskUpdate]:
        """Async generator for real-time task updates"""
        session = self.active_sessions.get(session_id)
        if not session:
            raise ValueError(f"Session {session_id} not found")
        
        async for message in session.send_message_stream(task):
            yield TaskUpdate(
                session_id=session_id,
                message_type=message.type,
                content=message.content,
                timestamp=message.timestamp,
                files_affected=message.files_affected
            )
```

### CLI Integration Patterns

**Headless Mode for Automation**:
```bash
# Direct CLI integration for simple tasks
claude -p "Analyze this file and suggest improvements" --project /path/to/project --output json

# Session resumption for continued work
claude --resume session-id-12345 -p "Continue with the refactoring"

# Batch processing with output capture
claude --batch-file tasks.txt --project /path/to/project --output-dir ./results
```

**Subprocess Management**:
```python
import subprocess
import asyncio
from pathlib import Path

class ClaudeCodeCLIManager:
    """Manage Claude Code CLI processes for delegation"""
    
    async def create_headless_session(self, config: SessionConfig) -> subprocess.Popen:
        """Create headless Claude Code session"""
        cmd = [
            'claude',
            '--project', config.working_directory,
            '--session-name', config.session_name,
            '--headless',  # No interactive mode
            '--output', 'json',  # Structured output
            '--log-level', 'info'
        ]
        
        if config.yolo_mode:
            cmd.append('--yolo')
        
        # Create process with proper I/O handling
        process = await asyncio.create_subprocess_exec(
            *cmd,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            cwd=config.working_directory
        )
        
        return process
    
    async def send_task_to_process(self, process: subprocess.Popen, task: str) -> TaskResult:
        """Send task to headless Claude Code process"""
        # Send task via stdin
        process.stdin.write(f"{task}\n".encode())
        await process.stdin.drain()
        
        # Read response from stdout
        response_data = []
        while True:
            line = await process.stdout.readline()
            if not line:
                break
            
            response_data.append(line.decode().strip())
            
            # Check for completion marker
            if line.decode().strip().endswith('</task_complete>'):
                break
        
        return TaskResult.from_cli_output(response_data)
```

## Log Monitoring and Activity Extraction

### JSONL Format Processing

**Structured Log Parsing**:
```python
import json
import asyncio
from pathlib import Path
from typing import AsyncIterator

class ClaudeCodeLogMonitor:
    """Monitor Claude Code JSONL transcript files"""
    
    def __init__(self, project_path: Path):
        self.project_path = project_path
        self.transcript_path = project_path / ".claude" / "transcript.jsonl"
    
    async def watch_session_activity(self, session_id: str) -> AsyncIterator[LogEntry]:
        """Watch JSONL logs for real-time activity"""
        session_log = self.project_path / ".claude" / "sessions" / f"{session_id}.jsonl"
        
        if not session_log.exists():
            raise FileNotFoundError(f"Session log not found: {session_log}")
        
        # Start watching from end of file
        with open(session_log, 'r') as f:
            f.seek(0, 2)  # Seek to end
            
            while True:
                line = f.readline()
                if line:
                    try:
                        log_data = json.loads(line.strip())
                        entry = LogEntry.from_dict(log_data)
                        yield entry
                    except json.JSONDecodeError:
                        continue  # Skip malformed lines
                else:
                    await asyncio.sleep(0.1)  # Wait for new content

class LogEntry:
    """Structured representation of Claude Code log entry"""
    
    def __init__(self, timestamp: str, session_id: str, message_type: str, 
                 content: str, tokens: dict = None, files_affected: list = None,
                 tool_name: str = None, metadata: dict = None):
        self.timestamp = timestamp
        self.session_id = session_id
        self.message_type = message_type  # 'user', 'assistant', 'system', 'tool_use', 'tool_result'
        self.content = content
        self.tokens = tokens or {}
        self.files_affected = files_affected or []
        self.tool_name = tool_name
        self.metadata = metadata or {}
    
    @classmethod
    def from_dict(cls, data: dict) -> 'LogEntry':
        """Create LogEntry from parsed JSON"""
        return cls(
            timestamp=data.get('timestamp'),
            session_id=data.get('session_id'),
            message_type=data.get('type'),
            content=data.get('content', ''),
            tokens=data.get('tokens'),
            files_affected=data.get('files_affected', []),
            tool_name=data.get('tool_name'),
            metadata=data.get('metadata', {})
        )
    
    def to_activity(self) -> 'Activity':
        """Convert log entry to Cafedelic activity"""
        activity_type = self.classify_activity_type()
        
        return Activity(
            session_id=self.session_id,
            type=activity_type,
            description=self.generate_description(),
            files_involved=self.files_affected,
            timestamp=self.timestamp,
            tokens_used=self.tokens.get('total', 0),
            metadata={
                'tool_name': self.tool_name,
                'message_type': self.message_type,
                **self.metadata
            }
        )
    
    def classify_activity_type(self) -> str:
        """Classify log entry into activity type"""
        if self.message_type == 'tool_use':
            return self.map_tool_to_activity(self.tool_name)
        elif self.files_affected:
            if 'read' in self.content.lower():
                return 'file_read'
            elif any(keyword in self.content.lower() for keyword in ['write', 'create', 'update']):
                return 'file_write'
            else:
                return 'file_access'
        elif self.message_type == 'assistant':
            if self.is_thinking_content():
                return 'thinking'
            else:
                return 'response'
        elif self.message_type == 'user':
            return 'user_input'
        else:
            return 'system_event'
    
    def map_tool_to_activity(self, tool_name: str) -> str:
        """Map tool usage to activity type"""
        tool_mapping = {
            'read_file': 'file_read',
            'write_file': 'file_write',
            'create_file': 'file_create',
            'search_files': 'search',
            'run_bash': 'command_execution',
            'git_commit': 'git_operation',
            'git_push': 'git_operation',
            'list_directory': 'directory_browse'
        }
        return tool_mapping.get(tool_name, 'tool_usage')
```

### Real-Time Activity Processing

**Activity Stream Processing**:
```python
class ActivityProcessor:
    """Process Claude Code activities for intelligence generation"""
    
    def __init__(self, database: CafedelicDatabase):
        self.database = database
        self.activity_handlers = {
            'file_write': self.handle_file_modification,
            'git_operation': self.handle_git_activity,
            'thinking': self.handle_reasoning_activity,
            'command_execution': self.handle_command_execution
        }
    
    async def process_activity_stream(self, session_id: str) -> AsyncIterator[ProcessedActivity]:
        """Process real-time activity stream from Claude Code"""
        log_monitor = ClaudeCodeLogMonitor(self.get_project_path(session_id))
        
        async for log_entry in log_monitor.watch_session_activity(session_id):
            activity = log_entry.to_activity()
            
            # Store raw activity
            await self.database.store_activity(activity)
            
            # Process activity for intelligence
            processed = await self.process_activity(activity)
            
            yield processed
    
    async def process_activity(self, activity: Activity) -> ProcessedActivity:
        """Process individual activity for intelligence insights"""
        handler = self.activity_handlers.get(activity.type, self.default_handler)
        intelligence = await handler(activity)
        
        return ProcessedActivity(
            activity=activity,
            intelligence=intelligence,
            processed_at=datetime.utcnow()
        )
    
    async def handle_file_modification(self, activity: Activity) -> ActivityIntelligence:
        """Generate intelligence for file modification activities"""
        file_paths = activity.files_involved
        
        insights = []
        for file_path in file_paths:
            # Analyze file impact
            impact = await self.analyze_file_impact(file_path)
            insights.append(impact)
            
            # Check for related sessions
            related_sessions = await self.find_sessions_working_on_file(file_path)
            if related_sessions:
                insights.append(f"File also being modified by sessions: {', '.join(related_sessions)}")
        
        return ActivityIntelligence(
            type='file_modification',
            insights=insights,
            coordination_opportunities=await self.find_coordination_opportunities(activity)
        )
    
    async def analyze_file_impact(self, file_path: str) -> str:
        """Analyze the impact of file modifications"""
        # Get file dependencies
        dependencies = await self.get_file_dependencies(file_path)
        
        if dependencies:
            return f"File {file_path} has {len(dependencies)} dependencies that may be affected"
        else:
            return f"File {file_path} appears to be self-contained"
    
    async def find_coordination_opportunities(self, activity: Activity) -> List[str]:
        """Find opportunities for session coordination"""
        opportunities = []
        
        # Check for sessions working on related files
        related_sessions = await self.database.find_sessions_by_file_overlap(
            activity.files_involved,
            exclude_session=activity.session_id
        )
        
        if related_sessions:
            opportunities.append(
                f"Consider coordinating with sessions: {', '.join(s.name for s in related_sessions)}"
            )
        
        # Check for similar ongoing tasks
        similar_tasks = await self.database.find_similar_active_tasks(activity.description)
        if similar_tasks:
            opportunities.append(
                f"Similar tasks detected in other sessions - potential for knowledge sharing"
            )
        
        return opportunities
```

## Session State Management and Context Preservation

### Session Lifecycle Management

**Comprehensive Session Tracking**:
```python
class SessionStateManager:
    """Manage Claude Code session state and context"""
    
    def __init__(self, database: CafedelicDatabase):
        self.database = database
        self.active_sessions = {}
        self.state_monitors = {}
    
    async def create_managed_session(self, config: SessionConfig) -> ManagedSession:
        """Create and start monitoring a Claude Code session"""
        # Create Claude Code session
        claude_session = await self.create_claude_code_session(config)
        
        # Create managed wrapper
        managed_session = ManagedSession(
            claude_session=claude_session,
            config=config,
            state=SessionState.STARTING,
            created_at=datetime.utcnow()
        )
        
        # Store in database
        await self.database.create_session(managed_session)
        
        # Start monitoring
        await self.start_session_monitoring(managed_session)
        
        self.active_sessions[managed_session.id] = managed_session
        return managed_session
    
    async def start_session_monitoring(self, session: ManagedSession):
        """Start comprehensive session monitoring"""
        # Monitor activity logs
        activity_task = asyncio.create_task(
            self.monitor_session_activity(session)
        )
        
        # Monitor process health
        health_task = asyncio.create_task(
            self.monitor_session_health(session)
        )
        
        # Monitor resource usage
        resource_task = asyncio.create_task(
            self.monitor_resource_usage(session)
        )
        
        self.state_monitors[session.id] = {
            'activity': activity_task,
            'health': health_task,
            'resources': resource_task
        }
    
    async def monitor_session_activity(self, session: ManagedSession):
        """Monitor session activity for state changes"""
        processor = ActivityProcessor(self.database)
        
        async for processed_activity in processor.process_activity_stream(session.id):
            # Update session state based on activity
            new_state = self.infer_session_state(processed_activity)
            
            if new_state != session.state:
                await self.update_session_state(session, new_state)
            
            # Check for completion indicators
            if self.is_task_complete(processed_activity):
                await self.handle_task_completion(session, processed_activity)
    
    def infer_session_state(self, activity: ProcessedActivity) -> SessionState:
        """Infer session state from activity"""
        if activity.activity.type == 'thinking':
            return SessionState.THINKING
        elif activity.activity.type in ['file_read', 'search']:
            return SessionState.ANALYZING
        elif activity.activity.type in ['file_write', 'command_execution']:
            return SessionState.IMPLEMENTING
        elif 'error' in activity.activity.content.lower():
            return SessionState.ERROR
        elif self.is_waiting_for_input(activity):
            return SessionState.WAITING_INPUT
        else:
            return SessionState.ACTIVE

class SessionState(Enum):
    STARTING = "starting"
    ACTIVE = "active"
    THINKING = "thinking"
    ANALYZING = "analyzing"
    IMPLEMENTING = "implementing"
    WAITING_INPUT = "waiting_input"
    ERROR = "error"
    STUCK = "stuck"
    COMPLETED = "completed"
    TERMINATED = "terminated"
```

### Context Capture and Handoff

**Session Context Snapshots**:
```python
class SessionContextManager:
    """Manage session context for handoffs and resumption"""
    
    async def capture_session_context(self, session_id: str) -> SessionContext:
        """Capture comprehensive session context"""
        session = await self.database.get_session(session_id)
        
        # Get recent activities
        recent_activities = await self.database.get_recent_activities(
            session_id, 
            since=datetime.utcnow() - timedelta(hours=2)
        )
        
        # Get open files and working state
        working_state = await self.capture_working_state(session)
        
        # Get conversation context via claude -p
        conversation_summary = await self.generate_conversation_summary(session_id)
        
        return SessionContext(
            session_id=session_id,
            timestamp=datetime.utcnow(),
            activities=recent_activities,
            working_state=working_state,
            conversation_summary=conversation_summary,
            git_state=await self.capture_git_state(session),
            environment_state=await self.capture_environment_state(session)
        )
    
    async def generate_conversation_summary(self, session_id: str) -> ConversationSummary:
        """Generate conversation summary using claude -p"""
        # Get transcript
        transcript = await self.get_session_transcript(session_id)
        
        # Use claude -p for analysis
        summary_prompt = f"""
        Analyze this Claude Code session transcript and provide:
        1. Current task/objective
        2. Progress made so far
        3. Key decisions and approaches taken
        4. Current challenges or blockers
        5. Next logical steps
        
        Transcript:
        {transcript}
        """
        
        summary_result = await self.call_claude_p(summary_prompt)
        
        return ConversationSummary(
            session_id=session_id,
            current_task=summary_result.get('current_task'),
            progress_summary=summary_result.get('progress'),
            key_decisions=summary_result.get('decisions', []),
            challenges=summary_result.get('challenges', []),
            next_steps=summary_result.get('next_steps', []),
            generated_at=datetime.utcnow()
        )
    
    async def handoff_session_context(self, from_session: str, to_session: str, 
                                     handoff_reason: str) -> HandoffResult:
        """Transfer context from one session to another"""
        # Capture source context
        source_context = await self.capture_session_context(from_session)
        
        # Generate handoff prompt
        handoff_prompt = self.generate_handoff_prompt(source_context, handoff_reason)
        
        # Send context to target session
        target_session = self.active_sessions.get(to_session)
        if not target_session:
            raise SessionNotFoundError(f"Target session {to_session} not found")
        
        # Send handoff message
        handoff_result = await self.send_handoff_message(target_session, handoff_prompt)
        
        # Record handoff in database
        await self.database.record_session_handoff(
            from_session=from_session,
            to_session=to_session,
            context=source_context,
            reason=handoff_reason,
            timestamp=datetime.utcnow()
        )
        
        return handoff_result
    
    def generate_handoff_prompt(self, context: SessionContext, reason: str) -> str:
        """Generate comprehensive handoff prompt"""
        return f"""
        I'm continuing work from another Claude Code session. Here's the context:
        
        **Previous Session Summary:**
        - Task: {context.conversation_summary.current_task}
        - Progress: {context.conversation_summary.progress_summary}
        
        **Key Decisions Made:**
        {chr(10).join(f"• {decision}" for decision in context.conversation_summary.key_decisions)}
        
        **Current Challenges:**
        {chr(10).join(f"• {challenge}" for challenge in context.conversation_summary.challenges)}
        
        **Files Being Worked On:**
        {chr(10).join(f"• {file}" for file in context.working_state.open_files)}
        
        **Git State:**
        - Branch: {context.git_state.current_branch}
        - Uncommitted changes: {len(context.git_state.modified_files)} files
        
        **Handoff Reason:** {reason}
        
        **Next Steps:** {chr(10).join(f"• {step}" for step in context.conversation_summary.next_steps)}
        
        Please acknowledge this context and continue the work accordingly.
        """
```

## Async Task Delegation Implementation

### Task Routing and Execution

**Intelligent Task Delegation**:
```python
class TaskDelegationEngine:
    """Core engine for intelligent task delegation"""
    
    def __init__(self, session_manager: SessionStateManager, 
                 context_manager: SessionContextManager):
        self.session_manager = session_manager
        self.context_manager = context_manager
        self.task_queue = asyncio.Queue()
        self.active_tasks = {}
    
    async def delegate_task(self, task: DelegationTask) -> DelegationResult:
        """Delegate task to most appropriate Claude Code session"""
        # Find best session for task
        target_session = await self.find_optimal_session(task)
        
        if not target_session:
            # Create new session if needed
            target_session = await self.create_session_for_task(task)
        
        # Execute delegation
        return await self.execute_delegation(task, target_session)
    
    async def find_optimal_session(self, task: DelegationTask) -> Optional[ManagedSession]:
        """Find the most suitable session for a task"""
        # Get available sessions
        available_sessions = [
            session for session in self.session_manager.active_sessions.values()
            if session.state in [SessionState.ACTIVE, SessionState.WAITING_INPUT]
        ]
        
        if not available_sessions:
            return None
        
        # Score sessions based on relevance
        session_scores = []
        for session in available_sessions:
            score = await self.calculate_session_relevance(session, task)
            session_scores.append((session, score))
        
        # Return highest scoring session
        if session_scores:
            session_scores.sort(key=lambda x: x[1], reverse=True)
            best_session, best_score = session_scores[0]
            
            # Only use session if score is above threshold
            if best_score > 0.3:  # 30% relevance threshold
                return best_session
        
        return None
    
    async def calculate_session_relevance(self, session: ManagedSession, 
                                        task: DelegationTask) -> float:
        """Calculate how relevant a session is for a task"""
        relevance_factors = []
        
        # File overlap relevance
        if task.related_files:
            session_files = await self.get_session_file_context(session.id)
            file_overlap = len(set(task.related_files) & set(session_files))
            file_relevance = file_overlap / max(len(task.related_files), 1)
            relevance_factors.append(('file_overlap', file_relevance, 0.4))
        
        # Task similarity relevance
        session_context = await self.context_manager.capture_session_context(session.id)
        if session_context.conversation_summary.current_task:
            task_similarity = await self.calculate_task_similarity(
                task.description,
                session_context.conversation_summary.current_task
            )
            relevance_factors.append(('task_similarity', task_similarity, 0.3))
        
        # Technology stack relevance
        if task.technology_stack:
            session_tech = await self.infer_session_technology(session.id)
            tech_overlap = len(set(task.technology_stack) & set(session_tech))
            tech_relevance = tech_overlap / max(len(task.technology_stack), 1)
            relevance_factors.append(('technology', tech_relevance, 0.2))
        
        # Session health and availability
        health_score = await self.get_session_health_score(session.id)
        relevance_factors.append(('health', health_score, 0.1))
        
        # Calculate weighted average
        total_score = sum(score * weight for _, score, weight in relevance_factors)
        return total_score
    
    async def execute_delegation(self, task: DelegationTask, 
                               session: ManagedSession) -> DelegationResult:
        """Execute task delegation to selected session"""
        # Create task tracking entry
        task_id = str(uuid.uuid4())
        self.active_tasks[task_id] = ActiveTask(
            id=task_id,
            session_id=session.id,
            description=task.description,
            started_at=datetime.utcnow(),
            status=TaskStatus.DELEGATING
        )
        
        try:
            # Send task to Claude Code session
            delegation_message = self.format_delegation_message(task)
            
            # Send via appropriate method (SDK or CLI)
            if session.communication_method == 'sdk':
                result = await self.delegate_via_sdk(session, delegation_message)
            else:
                result = await self.delegate_via_cli(session, delegation_message)
            
            # Update task status
            self.active_tasks[task_id].status = TaskStatus.IN_PROGRESS
            
            # Start monitoring for completion
            asyncio.create_task(self.monitor_task_progress(task_id))
            
            return DelegationResult(
                task_id=task_id,
                session_id=session.id,
                success=True,
                message="Task delegated successfully"
            )
            
        except Exception as e:
            # Handle delegation failure
            self.active_tasks[task_id].status = TaskStatus.FAILED
            self.active_tasks[task_id].error = str(e)
            
            return DelegationResult(
                task_id=task_id,
                session_id=session.id,
                success=False,
                error=str(e)
            )
    
    async def monitor_task_progress(self, task_id: str):
        """Monitor task progress and detect completion"""
        task = self.active_tasks.get(task_id)
        if not task:
            return
        
        # Monitor session activity for task completion
        start_time = task.started_at
        timeout = timedelta(minutes=30)  # 30-minute timeout
        
        while task.status == TaskStatus.IN_PROGRESS:
            # Check for timeout
            if datetime.utcnow() - start_time > timeout:
                task.status = TaskStatus.TIMEOUT
                break
            
            # Check session activity for completion indicators
            recent_activities = await self.database.get_recent_activities(
                task.session_id,
                since=start_time
            )
            
            if self.is_task_completed(recent_activities, task.description):
                task.status = TaskStatus.COMPLETED
                task.completed_at = datetime.utcnow()
                break
            
            await asyncio.sleep(5)  # Check every 5 seconds
        
        # Notify completion
        await self.handle_task_completion(task)

class DelegationTask:
    def __init__(self, description: str, related_files: List[str] = None,
                 technology_stack: List[str] = None, priority: str = 'normal',
                 timeout_minutes: int = 30):
        self.id = str(uuid.uuid4())
        self.description = description
        self.related_files = related_files or []
        self.technology_stack = technology_stack or []
        self.priority = priority
        self.timeout_minutes = timeout_minutes
        self.created_at = datetime.utcnow()

class TaskStatus(Enum):
    PENDING = "pending"
    DELEGATING = "delegating"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    TIMEOUT = "timeout"
```

## Integration with Cafedelic's MCP Architecture

### MCP Tool Implementation

**Delegation-Focused MCP Tools**:
```python
class CafedelicClaudeCodeMCPTools:
    """MCP tools for Claude Code integration"""
    
    def __init__(self, delegation_engine: TaskDelegationEngine):
        self.delegation_engine = delegation_engine
    
    @mcp_tool
    async def delegate_task_to_claude_code(self, task_description: str, 
                                         session_hint: str = None,
                                         related_files: List[str] = None) -> dict:
        """Delegate a task to an appropriate Claude Code session
        
        Args:
            task_description: Description of the task to delegate
            session_hint: Preferred session ID or name (optional)
            related_files: List of files related to the task (optional)
        
        Returns:
            Dict with task_id, session_id, and delegation status
        """
        task = DelegationTask(
            description=task_description,
            related_files=related_files or []
        )
        
        if session_hint:
            # Try to use hinted session
            target_session = await self.find_session_by_hint(session_hint)
            if target_session:
                result = await self.delegation_engine.execute_delegation(task, target_session)
            else:
                result = await self.delegation_engine.delegate_task(task)
        else:
            result = await self.delegation_engine.delegate_task(task)
        
        return {
            'task_id': result.task_id,
            'session_id': result.session_id,
            'success': result.success,
            'message': result.message
        }
    
    @mcp_tool
    async def get_task_status(self, task_id: str) -> dict:
        """Get current status of a delegated task
        
        Args:
            task_id: ID of the task to check
        
        Returns:
            Dict with task status and progress information
        """
        task = self.delegation_engine.active_tasks.get(task_id)
        if not task:
            return {'error': f'Task {task_id} not found'}
        
        return {
            'task_id': task.id,
            'session_id': task.session_id,
            'status': task.status.value,
            'description': task.description,
            'started_at': task.started_at.isoformat(),
            'progress': await self.calculate_task_progress(task)
        }
    
    @mcp_tool
    async def find_relevant_claude_code_sessions(self, query: str, limit: int = 5) -> dict:
        """Find Claude Code sessions relevant to a query
        
        Args:
            query: Search query for finding relevant sessions
            limit: Maximum number of sessions to return
        
        Returns:
            Dict with list of relevant sessions and their context
        """
        # Use semantic search on session contexts
        relevant_sessions = await self.database.search_session_contexts(
            query=query,
            limit=limit,
            include_summary=True
        )
        
        return {
            'query': query,
            'sessions': [
                {
                    'session_id': session.id,
                    'name': session.name,
                    'relevance_score': session.relevance_score,
                    'current_task': session.current_task,
                    'status': session.status,
                    'context_snippet': session.context_snippet
                }
                for session in relevant_sessions
            ]
        }
    
    @mcp_tool
    async def create_claude_code_session(self, project_path: str, session_name: str,
                                       task_type: str = 'general') -> dict:
        """Create a new Claude Code session for delegation
        
        Args:
            project_path: Path to the project directory
            session_name: Human-friendly name for the session
            task_type: Type of tasks this session will handle
        
        Returns:
            Dict with session information
        """
        config = SessionConfig(
            session_name=session_name,
            working_directory=project_path,
            task_type=task_type,
            session_id=str(uuid.uuid4())
        )
        
        session = await self.delegation_engine.session_manager.create_managed_session(config)
        
        return {
            'session_id': session.id,
            'session_name': session.name,
            'project_path': project_path,
            'status': session.state.value,
            'created_at': session.created_at.isoformat()
        }
```

This comprehensive integration architecture provides the foundation for seamlessly connecting Cafedelic's delegation platform with Claude Code's programmatic capabilities, enabling sophisticated task orchestration and session management.