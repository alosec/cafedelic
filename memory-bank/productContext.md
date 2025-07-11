# Product Context

## Why Cafedelic Exists (V3 Vision)

We're in a transformational moment where AI models can handle complex development tasks independently. The human role is shifting from typing to **task delegation, workflow orchestration, and async coordination**. Cafedelic builds the definitive task delegation platform for AI-assisted development - enabling rapid task assignment and intelligent session management.

## The Core Problem

**Traditional Development Tools**: Built for humans typing code
**AI-Native Development**: Humans need to delegate tasks, coordinate workflows, and manage async AI conversations

### The Paradigm Shift
- **Quick-chat delegation** - Natural language task assignment to Claude Code sessions
- **Task state orchestration** - Managing Planning/Analyzing/Implementing phases across sessions
- **Async coordination** - Background task processing with `claude --resume {session_id}`
- **Contextual session discovery** - find_relevant_chats() for intelligent session resumption

### Problems We Solve

1. **Task Assignment Friction**: No quick way to delegate tasks to appropriate Claude Code sessions
2. **Context Discovery Challenge**: Can't find relevant previous conversations for similar tasks
3. **Manual Workflow Management**: Constantly switching between planning, analysis, and implementation modes
4. **Session Coordination Overhead**: No async task delegation or automated handoffs between sessions
5. **Command Workflow Isolation**: Project-specific slash commands in ~/.claude/commands not integrated with delegation

## The Intelligence Gap

Current Claude Code usage patterns create systematic blind spots:
- **Session Isolation**: Each Claude conversation operates independently
- **Context Loss**: Project knowledge doesn't persist across sessions
- **Activity Invisibility**: Can't see what Claude is thinking or planning across instances
- **Manual Coordination**: Humans must manually manage session switching and context

Cafedelic bridges these gaps through async task delegation, contextual session discovery, and integration with project-specific commands in ~/.claude/commands/.

## How It Works (Intelligence-First Architecture)

### Quick-Chat Delegation Interface
```
Human: [Types in widget] "Implement user authentication"
Cafedelic: "Found similar context in auth-refactor [abc123]. Use existing session or create new?"
Human: [Clicks] "Use existing"
Cafedelic: "Task delegated to auth-refactor. Status: Planning... security patterns. Use /act when ready."

### Project Command Integration
```
Human: [Quick delegate] "Run my auth-tests command"
Cafedelic: "Found /auth-tests in ~/.claude/commands/. Delegating to test-session [def456]."
Claude (test-session): [Executes custom auth-tests workflow]

### SQLite Intelligence Database
```sql
-- Projects table tracks git repositories and worktrees
SELECT name, active_sessions FROM projects WHERE status = 'active';

-- Sessions table links Claude Code instances to projects  
SELECT session_name, project_name, last_activity FROM sessions 
JOIN projects ON sessions.project_id = projects.id;

-- Activities table captures all AI operations
SELECT COUNT(*) as claude_actions FROM activities 
WHERE type = 'file_read' AND created_at > datetime('now', '-1 hour');
```

### Display Adapter Flexibility
```javascript
// Terminal adapter (tmux integration)
const terminalAdapter = new TerminalAdapter(cafedelicDatabase);
terminalAdapter.displayProjectStatus('auth-refactor');

// VS Code extension adapter  
const vscodeAdapter = new VSCodeAdapter(cafedelicDatabase);
vscodeAdapter.showSessionSidebar();

// CLI adapter
const cliAdapter = new CLIAdapter(cafedelicDatabase);
cliAdapter.executeCommand('cafe status');
```

## Design Principles

1. **Database-First**: SQLite as single source of truth for all intelligence
2. **Conversation-Native**: MCP interface enables natural language interaction
3. **Enhancement over Replacement**: Integrates with existing tools, doesn't replace them
4. **Intelligence over Interface**: Focus on making AI work visible and manageable
5. **Assistant-Agnostic**: Architecture supports future expansion beyond Claude Code

## Evolution Path

### Phase 1: Claude Code Intelligence Foundation
- SQLite database schema for projects, sessions, activities
- MCP toolset for natural language project management
- Basic session tracking and context preservation

### Phase 2: Advanced Intelligence Features  
- `claude -p` integration for real-time AI activity summarization
- Cross-session context sharing and coordination
- Pattern recognition in AI development workflows

### Phase 3: Multi-Assistant Platform
- Support for additional AI assistants (Cline, future tools)
- Universal intelligence layer across different development environments
- Advanced orchestration and workflow automation