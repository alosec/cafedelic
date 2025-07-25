# Active Context

## Current State  
- **Date**: July 17, 2025
- **Focus**: TUI Data Integration - HTTP-based Claude Code Filesystem Connection
- **Status**: Database bypassed - TUI now connects directly to Claude Code filesystem via HTTP API

## V3 Paradigm Shift: Asynchronous Task Delegation Platform

### The Fundamental Reframe (July 11, 2025)
**From**: "Glass Box Mission Control for Observation"  
**To**: "Asynchronous Task Delegation Platform for AI Orchestration"

### Core Realization
AI models are now strong enough to handle complex tasks independently. The human interface should focus on:
- **Delegation**: Quick task assignment via natural language to appropriate Claude Code sessions
- **Orchestration**: Managing multiple AI conversations with task-state awareness  
- **Coordination**: Automated handoffs and context preservation between sessions
- **Monitoring**: Real-time task progress tracking and completion notifications

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

### The Delegation Platform Vision
```
Quick-Chat: [Implement auth] → [auth-session] | Status: Planning... | Progress: [████░░░░] 40%
```
Focus on async task orchestration with `claude --resume {session_id}` integration for background AI coordination.

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
## Current Implementation Focus (Database Integration Challenge - July 15, 2025)

### Breakthrough: Database-Free Integration Complete
**Solution**: Direct HTTP API connection to Claude Code filesystem
**Achievement**: TUI now shows real projects/sessions without database dependency
**Status**: Task delegation platform functional with live data

### HTTP-Based Integration Architecture
**Approach**: Bypass database entirely, connect TUI directly to Claude Code data
- **HTTP API**: Express server provides `/api/projects` and `/api/sessions` endpoints
- **Direct Filesystem Access**: claude-code-service.ts reads Claude Code's native storage
- **Real-Time Data**: TUI displays actual projects and sessions from filesystem
- **No Database Required**: Eliminates database population complexity
- **Immediate Functionality**: Task delegation platform works with real data

### Technical Implementation Completed
**claude-code-service.ts**: New service layer for Claude Code filesystem access
**HTTP API Endpoints**: `/api/projects`, `/api/sessions`, `/api/sessions/summary`
**TUI HTTP Integration**: cafe_ui/adapter.py uses requests to fetch real data
**Mock Data Replacement**: claude_code_data.py replaced mock_data.py for real data access

### Current Integration Status
✅ **HTTP API Connection**: TUI successfully connects to Claude Code filesystem
✅ **Real Data Display**: Projects and sessions from actual Claude Code storage
✅ **Service Layer**: claude-code-service.ts provides formatted data for TUI
✅ **TUI Functional**: Task delegation platform works with live data
❌ **Activity Monitoring**: No real-time Claude Code activity ingestion yet
❌ **Intelligence Generation**: No claude -p analysis of session data yet

### Current Priority: Intelligence Layer Development
1. **Session Activity Monitoring**: Implement real-time activity tracking
2. **Context Analysis**: Add claude -p intelligence generation
3. **Cross-Session Coordination**: Pattern recognition across sessions
4. **Task State Management**: Real task delegation with status tracking
5. **Background Processing**: Async task execution and monitoring
6. **Advanced TUI Features**: Context-aware session management

### Technical Foundation Complete
✅ **Discovery System**: Complete JSONL parser extracting session metadata
✅ **HTTP Integration**: Direct filesystem access bypassing database complexity
✅ **TUI Framework**: Functional delegation platform with real data display
✅ **Service Layer**: claude-code-service.ts provides formatted data for consumption
✅ **MCP Integration**: Server foundation for conversational interface

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