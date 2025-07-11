# Active Context

## Current State  
- **Date**: July 11, 2025
- **Focus**: V3 Architectural Exploration - Glass Box Mission Control
- **Status**: Contemplative inquiry on fundamental UI paradigm and separation of concerns

## V3 Paradigm Shift: Glass Box Mission Control

### The Fundamental Reframe (July 11, 2025)
**From**: "Tmux IDE with AI Features"  
**To**: "Glass Box Mission Control for AI-Assisted Development"

### Core Realization
AI models are now strong enough to do 100% of editing work. The human interface should focus on:
- **Navigation**: Moving through projects and contexts
- **Observation**: Watching AI work in real-time  
- **Orchestration**: Managing Claude Code sessions across projects
- **Guidance**: High-level direction, not low-level typing

### The Tmux Reality Check
Attempting to build modern interactive UI with tmux is fundamentally mismatched:
- tmux = "rebar and duct tape" 
- Modern UI needs = "React/HTML/CSS level sophistication"
- Result: Fighting the wrong battle

### Architectural Separation Strategy
**Intelligence Layer** (Cafedelic's core value):
- Observability pipeline (WTE pattern)
- claude -p context reasoning and task summarization
- Session management via SQLite
- Project/worktree orchestration

**Display Layer** (User's responsibility):
- Their own tmux configuration
- Terminal multiplexer preferences  
- Editor choice and setup

### The Glass Box Vision
```
(Dynamic file tree) | "Claude is now..." (claude -p summary) | Session: marcel (tracked in SQLite)
```
Focus on making AI work visible and comprehensible rather than replacing terminal UI.

## Previous Architectural Decisions (V2 Foundation)

### Keeping All MCP Tools
Decision: Maintain all 8 existing MCP tools rather than deprecating
- MCP tools remain available for programmatic access
- CLI commands will call bash scripts directly for performance
- This provides maximum flexibility for users

### CLI-First Architecture (UPDATED SCOPE)
Decision: Create 'cafe' command suite as primary interface
- `cafe init`: Ensure server running and operational, return errors if missing dependencies
- `cafe deploy`: Deploy simple 2-pane layout (70% emacs top, 30% system events bottom)
- `cafe session`: Manage Claude Code instances (future)
- `cafe status`: Live activity dashboard (future) 
- `cafe events`: Query and display system events (future)

### Direct Script Invocation
Decision: CLI bypasses MCP layer, calls scripts directly
- Example: `cafe pane assign` → `assign-properties.sh`
- Removes HTTP/stdio overhead
- Simpler error handling
- Faster execution

### System Events Database Design
Decision: SQLite for system events, not flat files
- Schema: id, timestamp, source, level, message, session_name
- Enables rich queries across sessions
- Supports real-time reactive displays
- One-liner system events for clarity

### Layout Specification (SIMPLIFIED SCOPE)
Decision: 70/30 split as default (simplified from 3-pane)
```
┌─────────────────────────────────────┐
│            Emacs Editor             │
│        (Claude Desktop's            │  
│          primary editor)            │
│             (70%)                   │
├─────────────────────────────────────┤
│          System Events              │
│     (Activity, Logs, Status)        │
│             (30%)                   │
└─────────────────────────────────────┘
```
## Claude Code SDK Insights
- Official SDK released May 2025, replacing deprecated @anthropic-ai/claude-code package
- Enables subprocess control of Claude Code sessions
- Can build reactive status displays using SDK
- Integration point for session management
- Supports TypeScript and Python bindings

## Session Management Research
- claude-squad provides tmux-based multi-session management
- CCmanager offers tmux-free alternative with TUI
- Both track sessions by ID with human-friendly names
- We'll integrate best patterns from both approaches

## Previous Completed Work (Foundation)

### Property System Architecture
The system uses orthogonal dimensions for pane management:
- **@source**: "user" | "claude-desktop" | "claude-code" | "system"
- **@role**: "editor" | "terminal" | "logs" | "tests" | "debug" | "monitor"
- **@name**: Human-friendly names for backward compatibility

### V2 WTE Implementation
```typescript
// Core pattern - entire system in ~150 lines
pipe(
  mcpLogWatcher(logPath),
  fileOperationTransform,
  emacsExecutor(scriptPath)
);
```

### Available MCP Tools (All Retained)
1. `assign_pane_properties` - Set name, source, and/or role on a pane
2. `list_panes_by_properties` - Filter panes by any property combination
3. `find_pane_by_source_and_role` - Direct lookup with exact match
4. `capture_pane_with_properties` - Advanced pane capture with property filtering
5. `send_keys_to_pane` - Send text/keys to named pane
6. `send_special_key_to_pane` - Send special keys (enter, ctrl-c, etc.)
7. `send_ctrl_c_to_pane_by_name` - Send Ctrl-C with double-tap option
8. `get_details_for_pane_by_name` - Get pane information
## Current Implementation Focus (Post-Exploration)

### Resolved Architecture: MCP Intelligence Platform
**Decision**: Cafedelic is an MCP server providing intelligence layer for Claude Code project/session management
- **Core**: SQLite database as single source of truth
- **Interface**: Natural language MCP tools for conversational management
- **Display**: Plugin adapter system supporting terminal, VS Code, web interfaces
- **Scope**: Intelligence and coordination, not terminal UI replacement

### Phase 1 Implementation Priorities (UPDATED - Textual Integration)
1. **Database Schema Implementation**: SQLite tables for projects, sessions, activities, context
2. **Core MCP Tools**: project_tools.ts, session_tools.ts, intelligence_tools.ts  
3. **Claude Code Integration**: Process monitoring, log parsing, activity extraction
4. **Textual UI Platform**: Rich terminal/web UI for project management (BREAKTHROUGH)
5. **Intelligence Pipeline**: claude -p integration for context analysis

### Textual Integration Discovery (July 11, 2025)
**Platform**: https://github.com/Textualize/textual - Python TUI framework
**Capabilities**: Rich terminal UIs that can serve to web, reactive/dynamic, professional-grade
**Perfect Fit**: Ideal platform for cafedelic's intelligence layer visualization
**Integration**: Textual as primary display adapter, reads from SQLite intelligence database

### Technical Architecture Decisions (Finalized)
1. **Database-First**: All state lives in SQLite, no in-memory persistence
2. **MCP-Native**: Primary interface through conversational tools, not CLI
3. **Plugin Display**: Separate intelligence from presentation via adapter pattern
4. **Assistant-Agnostic**: Database schema supports future expansion beyond Claude Code
5. **Enhancement Philosophy**: Integrate with existing tools, don't replace them

## V2 Foundation (Proven & Retained)
### Phase 1: cafe CLI Foundation (COMPLETED ✅)
- [x] Main cafe entry script with subcommand routing
- [x] cafe init - Validate server running, check dependencies
- [x] cafe deploy - Simple 2-pane layout (70% emacs, 30% system events)  
- [x] Direct script invocation pattern
- [x] Basic error handling and user feedback
- [x] PATH installation system

### Technical Assets Available
- **WTE Pipeline**: Proven event-driven architecture (~150 lines)
- **Property System**: Multi-dimensional pane management (source/role/name)
- **MCP Tools**: 8 working tools for programmatic pane access
- **Script Library**: Robust bash scripts for tmux operations
- **SQLite Foundation**: Basic database patterns established

## Key Technical Decisions Summary
- **CLI over MCP**: Direct user interface via cafe commands
- **Scripts over Servers**: Bypass MCP layer for CLI operations
- **Database over Files**: SQLite for structured system event data
- **Names over IDs**: Human-friendly session identification
- **Reactive over Push**: Database-backed displays that poll