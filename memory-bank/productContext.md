# Product Context

## Why Cafedelic Exists

AI development tools operate as black boxes. When Claude Desktop works on your code, you see results but not the journey. Cafedelic makes the invisible visible while providing control over how and where insights appear.

## The Evolution

Cafedelic started as a transparency framework but evolved into something more powerful: an integrated AI pair programming environment. We kept the transparency features but added active workspace management, transforming from passive observer to active IDE.

### Problems We Solve

1. **Session Chaos**: Multiple Claude Code instances without organization
2. **Context Switching**: Constant jumping between terminal, editor, logs
3. **Lost Activity**: No unified view of what AI and developers are doing
4. **Complex Setup**: Too many commands to create productive workspace
5. **Static Interfaces**: Can't see live updates across tools

## The Integration Gap

Current tools provide either:
- **Too Fragmented**: Separate terminals for each Claude Code session
- **Too Manual**: Constant tmux commands and pane management
- **Too Opaque**: No visibility into AI activity across sessions

Cafedelic provides seamless integration - one command to productive IDE.

## How It Works

### One Command IDE
```bash
cafe init  # Instant 30/70/30 layout with file tree, editor, and messages
```

### Session Management  
```bash
cafe session new frontend-refactor  # Start Claude Code with friendly name
cafe session list                   # See all active sessions
cafe status                        # Live dashboard of AI activity
```

### Direct Integration
- No MCP server overhead for CLI commands
- Bash scripts called directly for speed
- SQLite system events database for rich querying
- Reactive displays that update automatically

## The 30/70/30 Philosophy

Our default layout emerged from real usage patterns:
```
┌─────────────┬─────────────────────┐
│  Directory  │       Editor        │
│    Tree     │      (Emacs)        │
│    (30%)    │       (70%)         │
├─────────────┴─────────────────────┤
│         System Events              │
│     (Terminal, Logs, Activity)     │
│             (30%)                  │
└────────────────────────────────────┘
```
- **30% File Tree**: Always see project structure
- **70% Editor**: Maximum space for code work  
- **30% System Events**: Unified activity stream from all sources

## User Experience

### For Developers
Simple commands, powerful results:
```
[Starting work]
$ cafe init
→ Full IDE layout in 1 second

$ cafe session new auth-refactor
→ Claude Code starts with friendly name

[During work]
System Events panel shows:
10:32:15 [claude-code] Started auth-refactor session
10:32:18 [activity] Claude reading src/auth/login.js
10:32:20 [terminal] Running: npm test auth
10:32:25 [logs] All auth tests passing

[Managing sessions]
$ cafe status
→ See all Claude Code sessions and activity
```

### Flexible Configuration
```bash
# Use different layouts
cafe make --layout minimal    # Just editor and terminal
cafe make --layout full      # 5-pane comprehensive

# Direct pane management via CLI
cafe pane assign editor --source claude-code
cafe pane send events "Starting new feature"
```

## Design Principles

1. **CLI First**: Everything starts with simple cafe commands
2. **Direct Execution**: Skip middleware, call scripts directly
3. **Session Oriented**: Human names for Claude Code instances
4. **Database Backed**: Rich querying and history, not just text
5. **Tmux Native**: Deep integration, not abstraction

## Evolution Path

### Today: Claude Code IDE
- Multiple Claude Code sessions with names
- Unified activity tracking
- One-command workspace setup

### Tomorrow: Intelligent Orchestration
- AI agents that coordinate
- Pattern learning from sessions
- Suggested next actions

### Future: Adaptive Platform
- Learns your development patterns
- Optimizes layouts for tasks
- Predictive session management