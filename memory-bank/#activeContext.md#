# Active Context

## Current State
- **Date**: June 12, 2025
- **Focus**: Building cafe CLI tool suite for Claude Code IDE
- **Status**: Architecture defined, implementation starting

## Recent Architectural Decisions

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
## Implementation Priorities

### Phase 1: cafe CLI Foundation (IMMEDIATE SCOPE)
1. Main cafe entry script with subcommand routing
2. `cafe init` - Ensure server running, check dependencies, return errors
3. `cafe deploy` - Simple 2-pane layout (70% emacs, 30% system events)
4. Direct bash script invocation pattern
5. Basic error handling and validation

### Phase 2: System Events Database (NEXT)
1. SQLite schema for one-liner system event tracking
2. cafe events command with --follow mode
3. Integration points for various sources
4. Reactive display updates

### Phase 3: Claude Code Integration (FUTURE)
1. Session management with human names
2. SDK subprocess integration
3. Multi-session status dashboard
4. Activity aggregation across sessions

## Next Session Tasks (PHASE 1 COMPLETE ✅)
1. ✅ Create cli/cafe main entry script with init/deploy commands
2. ✅ Implement cafe init for server validation and dependency checking
3. ✅ Implement cafe deploy for 2-pane layout (70% emacs, 30% system events)
4. ✅ Test direct script invocation pattern
5. ✅ Validate emacs integration and system events display

## Next Priorities (PHASE 2)
1. Build system events database schema and reactive viewer
2. Integrate WTE pipeline with system events display
3. Add cafe events command with --follow mode
4. Enhance cafe deploy with layout options
5. Add session tracking preparation for Claude Code integration

## Key Technical Decisions Summary
- **CLI over MCP**: Direct user interface via cafe commands
- **Scripts over Servers**: Bypass MCP layer for CLI operations
- **Database over Files**: SQLite for structured system event data
- **Names over IDs**: Human-friendly session identification
- **Reactive over Push**: Database-backed displays that poll