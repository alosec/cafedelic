# Implement Configurable Pane Routing for Shared Visibility System

## Overview

Cafedelic V2 has successfully implemented the WTE (Watch-Transform-Execute) pattern, reducing our codebase from ~2000 to ~150 lines while maintaining all functionality. However, the current implementation uses fixed pane destinations. We need to make this fully configurable via MCP tools to achieve our vision of a shared visibility system for Claude Desktop and Claude Code.

## Current State

- ✅ WTE pattern implemented and working
- ✅ File operation watching from MCP logs
- ✅ Claude Code operation watching
- ✅ Emacs integration functional
- ❌ No dynamic pane configuration
- ❌ No MCP tools for routing control
- ❌ Missing pane interaction capabilities

## Vision

Create a configurable shared visibility system where:
1. **Immediate**: Claude Desktop/Code actions are visible in user-configured tmux panes
2. **Near-term**: Different types of output (files, logs, activity) route to different panes
3. **Future**: Full workspace orchestration ("set up workspace for marcel in window 3")

## Implementation Plan

### Phase 1: Core MCP Tools (Priority)
Implement essential pane management tools as simple sh scripts:

**Pane Management Tools:**
- [ ] `assign_name_to_pane` - Name a pane for easy reference
- [ ] `read_pane_by_name` - Read content from named panes
- [ ] `send_to_pane` - Send text to named panes
- [ ] `list_named_panes` - Show all named panes
- [ ] `get_pane_details` - Get info about a named pane
- [ ] `unname_pane` - Remove a pane's custom name

**Pane Interaction Tools:**
- [ ] `send_special_key_to_pane` - Send special keys (Enter, Escape, Tab, arrows, etc.)
- [ ] `send_ctrl_c_to_pane` - Send Ctrl-C with optional double-tap for Claude Code
- [ ] `clear_pane` - Clear a pane's content
- [ ] `send_key_sequence` - Send complex key combinations

**Routing Configuration Tools:**
- [ ] `set_output_destination` - Configure where specific output types go
- [ ] `get_routing_config` - Show current routing configuration
- [ ] `clear_routing` - Reset routing configuration
- [ ] `assign_pane_role` - Assign semantic roles (editor, logs, terminal)

### Phase 2: Shell Script Foundation
Create robust sh scripts that handle tmux operations:

```bash
# scripts/pane-management/send-special-key.sh
#!/bin/bash
PANE_NAME=$1
KEY=$2

# Get pane coordinates from name registry
COORDS=$(grep "=$PANE_NAME$" ~/.cafedelic/pane-names | cut -d= -f1)
if [ -z "$COORDS" ]; then
    echo "Error: Pane '$PANE_NAME' not found"
    exit 1
fi

# Map friendly names to tmux key names
case "$KEY" in
    enter) TMUX_KEY="Enter" ;;
    escape) TMUX_KEY="Escape" ;;
    tab) TMUX_KEY="Tab" ;;
    ctrl-c) TMUX_KEY="C-c" ;;
    ctrl-d) TMUX_KEY="C-d" ;;
    ctrl-z) TMUX_KEY="C-z" ;;
    up|down|left|right) TMUX_KEY="${KEY^}" ;;
    page-up) TMUX_KEY="PageUp" ;;
    page-down) TMUX_KEY="PageDown" ;;
    *) TMUX_KEY="$KEY" ;;
esac

tmux send-keys -t "$COORDS" "$TMUX_KEY"
```

```bash
# scripts/pane-management/send-ctrl-c.sh
#!/bin/bash
PANE_NAME=$1
DOUBLE_TAP=$2

COORDS=$(grep "=$PANE_NAME$" ~/.cafedelic/pane-names | cut -d= -f1)
if [ -z "$COORDS" ]; then
    echo "Error: Pane '$PANE_NAME' not found"
    exit 1
fi

tmux send-keys -t "$COORDS" C-c

# For Claude Code, send C-c twice quickly
if [ "$DOUBLE_TAP" = "true" ]; then
    sleep 0.1
    tmux send-keys -t "$COORDS" C-c
fi
```

### Phase 3: WTE Pipeline Integration
Enhance existing pipelines with routing configuration:

```typescript
// watchers/routing-config.ts
export async function* routingConfigWatcher() {
  const configPath = '~/.cafedelic/routing-config';
  // Watch for changes and yield routing updates
}

// transforms/route-by-type.ts
export function routeByType(routing: RoutingConfig) {
  return (operation: Operation) => {
    const destination = routing.getDestination(operation.type);
    return { ...operation, targetPane: destination };
  };
}

// executors/pane-output.ts
export function paneExecutor() {
  return async (op: RoutedOperation) => {
    if (op.targetPane) {
      await execAsync(`scripts/send-to-pane.sh "${op.targetPane}" "${op.content}"`);
    }
  };
}
```

### Phase 4: tmux-mcp Integration Strategy
Consider integration with tmux-mcp server for advanced features:

1. **Hybrid Approach**: 
   - Cafedelic handles routing logic and orchestration
   - tmux-mcp provides low-level tmux operations
   - Shell scripts for simple, reliable operations

2. **Future Capabilities**:
   - Use tmux-mcp for complex session management
   - Leverage its command execution features
   - Maintain our simple sh scripts for core operations

## Technical Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│  Claude Desktop │────▶│    Cafedelic     │────▶│   tmux panes    │
│  Claude Code    │     │  (WTE Pipelines) │     │   (visible)     │
└─────────────────┘     └──────────────────┘     └─────────────────┘
         │                       │                         ▲
         │                       │                         │
         ▼                       ▼                         │
┌─────────────────┐     ┌──────────────────┐             │
│   MCP Tools     │     │  Shell Scripts   │─────────────┘
│  (configure)    │     │   (execute)      │
└─────────────────┘     └──────────────────┘
```

## Implementation Details

### Storage Structure
```
~/.cafedelic/
├── pane-names          # name=session:window.pane mappings
├── routing-config      # type=destination mappings
├── pane-roles         # pane=role assignments
└── active-routes      # runtime routing state
```

### Script Organization
```
cafedelic/scripts/
├── pane-management/
│   ├── assign-name.sh
│   ├── read-pane.sh
│   ├── send-to-pane.sh
│   ├── send-special-key.sh
│   ├── send-ctrl-c.sh
│   ├── clear-pane.sh
│   └── list-panes.sh
├── routing/
│   ├── set-destination.sh
│   ├── get-config.sh
│   ├── assign-role.sh
│   └── clear-routes.sh
└── utils/
    ├── find-pane.sh
    └── validate-coords.sh
```

### MCP Tool Examples

```typescript
// Assign a friendly name to a pane
await assignNameToPane({
  session: '0',
  window: 1,
  pane: 2,
  name: 'editor-output'
});

// Send special keys
await sendSpecialKeyToPane({
  name: 'editor-output',
  key: 'ctrl-c'
});

// Configure routing
await setOutputDestination({
  type: 'file-operations',
  pane: 'editor-output'
});

// Send Ctrl-C twice for Claude Code
await sendCtrlCToPane({
  name: 'claude-code-pane',
  double_tap: true
});
```

## Success Criteria

- [ ] Can assign names to tmux panes via MCP tools
- [ ] Can send text and special keys to named panes
- [ ] Can configure routing destinations for different output types
- [ ] File operations appear in user-configured panes
- [ ] Can interact with Claude Code (double Ctrl-C support)
- [ ] Configuration persists across restarts
- [ ] Zero hard-coded pane assumptions
- [ ] Works alongside tmux-mcp if present

## Example Workflow

```bash
# 1. Name your panes
claude> assignNameToPane({ session: '0', window: 1, pane: 0, name: 'main-editor' })
claude> assignNameToPane({ session: '0', window: 1, pane: 1, name: 'activity-log' })
claude> assignNameToPane({ session: '0', window: 1, pane: 2, name: 'claude-code' })

# 2. Configure routing
claude> setOutputDestination({ type: 'file-operations', pane: 'main-editor' })
claude> setOutputDestination({ type: 'activity-summary', pane: 'activity-log' })

# 3. Now all file operations show in main-editor pane
# Activity summaries appear in activity-log pane

# 4. Control Claude Code
claude> sendCtrlCToPane({ name: 'claude-code', double_tap: true })
```

## Related Context

- Previous v2 redesign: #11
- WTE pattern documentation: memory-bank/systemPatterns.md
- Deli reference implementation: /home/alex/code/deli
- tmux-mcp integration option: https://github.com/nickgnd/tmux-mcp

## Notes

This implementation maintains our philosophy of simple, composable tools while adding powerful interaction capabilities. The shell script approach ensures reliability and easy debugging, while the optional tmux-mcp integration provides a path to more advanced features. Each piece does one thing well, and they combine to create powerful workflows for shared visibility between Claude and the developer.
