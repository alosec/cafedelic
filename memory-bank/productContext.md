# Product Context

## Why Cafedelic Exists (V3 Vision)

We're in a transitional moment where AI models are strong enough to do 100% of code editing. The human role is shifting from typing to **navigation, observation, orchestration, and guidance**. Cafedelic builds "glass box mission control" for AI-assisted development - making AI work visible and manageable across projects.

## The Core Problem

**Traditional Development Tools**: Built for humans typing code
**AI-Native Development**: Humans need to navigate, observe, and orchestrate AI work

### The Paradigm Shift
- **Editing interface becomes irrelevant** - AI does the typing
- **Mission control becomes critical** - Humans need to see and guide
- **Project context matters most** - Managing multiple AI sessions across codebases
- **Observability is the new editing** - Understanding what AI is doing and why

### Problems We Solve

1. **AI Work Invisibility**: Can't see what Claude is thinking or planning
2. **Session Chaos**: Multiple Claude Code instances without project context
3. **Context Fragmentation**: AI loses track across files and conversations  
4. **Manual Orchestration**: Constantly managing AI sessions and project switching
5. **Intelligence Isolation**: Each AI conversation exists in a silo

## The Intelligence Gap

Current Claude Code usage patterns create systematic blind spots:
- **Session Isolation**: Each Claude conversation operates independently
- **Context Loss**: Project knowledge doesn't persist across sessions
- **Activity Invisibility**: Can't see what Claude is thinking or planning across instances
- **Manual Coordination**: Humans must manually manage session switching and context

Cafedelic bridges these gaps through comprehensive session intelligence and orchestration.

## How It Works (Intelligence-First Architecture)

### MCP Conversational Interface
```
Human: "What are my active projects?"
Orchestrator Claude: "You have 3 projects: auth-refactor (2 sessions), ui-components (1 session), db-migration (paused). Marcel is stuck on OAuth middleware - want me to open that session?"

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