# Active Context

## Current State (2025-05-27)

### Working Features ✅

1. **MCP Log Monitoring**: Successfully watches Claude Desktop logs at `/home/alex/.config/Claude/logs/`
2. **Dynamic Output Routing**: RoutingManager allows flexible pane assignment
3. **Emacs Integration**: Auto-opens files/directories via pane-specific servers
4. **TMEX Layout Tools**: MCP tools for layout management (create, capture, clear)
5. **Activity Tracking**: Human-readable translations of AI actions
6. **Shell Script Integration**: Reliable execution via script wrappers

### Available MCP Tools

**Routing & Configuration**:
- `setEditorDestination(paneSpec)` - Assign output to any pane
- `getRoutingAssignments()` - View current routing
- `clearRoutingAssignment(role)` - Clear assignments
- `get_active_context()` - Get recent activity summary

**Layout Management**:
- `create_tmex_layout(targetPane, layout)` - Deploy TMEX layouts
- `capture_layout_state(target?)` - Get geometric analysis
- `clear_tmux_panes(target, mode, verify?)` - Clear with strategies

**Emacs Integration**:
- `toggle_auto_open(enable?)` - Control auto file opening
- `get_emacs_status(detailed?)` - Check integration status
- `set_emacs_mode(mode)` - Configure emacs mode
- `get_pane_servers_status()` - View pane server status

### Current Architecture

```
Claude Desktop MCP Logs → DesktopMCPWatcher → Event Bus
                                    ↓
                            TranslatorService → Human Insights
                                    ↓
                            RoutingManager → User's Tmux Pane
                                    ↓
                            Emacs Integration → File Opens
```

### Configuration Example

Current working setup:
```bash
# User has session 0 with 2 panes
tmux list-panes -t 0
# 0: [180x47] (active)
# 1: [180x47]

# Set cafedelic output to pane 1
setEditorDestination("0:0.1")

# Files now auto-open in pane 1's emacs
```

### Recent Fixes

1. **Correct Log Location**: Fixed from VS Code logs to Claude Desktop logs
2. **Dynamic Routing**: Replaced hard-coded session 9 assumptions
3. **Shell Script Integration**: Bypassed MCP parameter passing issues
4. **Flexible Assignment**: Works with any tmux layout

### Known Issues

1. **No Persistence**: Routing assignments lost on restart
2. **Limited Error Recovery**: Manual intervention needed for some failures
3. **No Tests**: Implementation lacks test coverage
4. **Memory Only**: No database integration yet

### Next Focus Area

**Pane Display Abstraction** (Issue #7):
- Unified API for all pane updates
- State verification and recovery
- Replace direct tmux commands
- Foundation for advanced UI features

## Active Development Patterns

### Shell Script Wrapper Pattern
```typescript
// MCP tools wrap proven shell scripts
export async function setEditorDestination(params) {
  const scriptPath = '/home/alex/code/cafedelic/scripts/emacs/pane-server/start-pane-emacs.sh';
  return await executeScript(scriptPath, params);
}
```

### Event-Driven Updates
```typescript
// Services communicate via events, not direct calls
watcher.on('mcp-entry', (entry) => {
  translator.translate(entry);
  router.route(translation);
});
```

### User-First Configuration
- No hard-coded pane names or sessions
- Dynamic discovery and assignment
- Graceful fallbacks
- Clear error messages
