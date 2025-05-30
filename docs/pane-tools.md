# Cafedelic Pane Management Tools

## Overview

Cafedelic now includes powerful pane management tools that allow Claude Desktop and Claude Code to interact with tmux panes dynamically. No more hard-coded pane destinations!

## Quick Start

### 1. Name Your Panes

```bash
# In Claude Desktop, use these MCP tools:
assignNameToPane({ session: '0', window: 0, pane: 0, name: 'editor' })
assignNameToPane({ session: '0', window: 0, pane: 1, name: 'logs' })
assignNameToPane({ session: '0', window: 1, pane: 0, name: 'claude-code' })
```

### 2. Configure Routing

```bash
# Route different output types to named panes
setOutputDestination({ type: 'files', pane: 'editor' })
setOutputDestination({ type: 'activity', pane: 'logs' })
```

### 3. Interact with Panes

```bash
# Send text
sendToPane({ name: 'editor', text: 'Hello from Claude!' })

# Send special keys
sendSpecialKeyToPane({ name: 'editor', key: 'ctrl-s' })

# Exit Claude Code
sendCtrlCToPane({ name: 'claude-code', double_tap: true })

# Read pane content
readPaneByName({ name: 'logs', lines: 50 })
```

## Available MCP Tools

### Pane Naming
- `assignNameToPane` - Give a pane a friendly name
- `listNamedPanes` - See all named panes

### Pane Interaction
- `sendToPane` - Send text to a named pane
- `sendSpecialKeyToPane` - Send keys like Enter, Escape, Ctrl-C
- `sendCtrlCToPane` - Send Ctrl-C with optional double-tap
- `readPaneByName` - Read content from a pane

### Routing Configuration
- `setOutputDestination` - Configure where output types go
- `getRoutingConfig` - View current routing setup

## Shell Scripts

All MCP tools are backed by simple shell scripts in `scripts/`:

```
scripts/
├── pane-management/
│   ├── assign-name.sh      # Name a pane
│   ├── send-to-pane.sh      # Send text
│   ├── send-special-key.sh  # Send special keys
│   ├── send-ctrl-c.sh       # Send Ctrl-C
│   ├── read-pane.sh         # Read pane content
│   └── list-named-panes.sh  # List all named panes
└── routing/
    ├── set-output-destination.sh  # Configure routing
    └── get-routing-config.sh      # Show routing
```

## Configuration Storage

All configuration is stored in `~/.cafedelic/`:
- `pane-names` - Maps names to pane coordinates
- `routing-config` - Maps output types to pane names

## Integration with WTE Pipelines

The routing system integrates seamlessly with Cafedelic's Watch-Transform-Execute pipelines:

```typescript
// Routing is now dynamic!
const targetPane = await getTargetPane('files');
if (targetPane) {
  await sendToPane(targetPane, fileContent);
}
```

## Testing

Run the test script to verify everything works:

```bash
npm run test-pane-tools
```

This will test all pane management features and show you the results.
