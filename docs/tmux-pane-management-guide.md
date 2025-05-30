# Tmux Pane Management Guide for Cafedelic MCP Server

This guide explains how to use Cafedelic's MCP (Model Context Protocol) server to manage tmux panes for output assignment and routing. The system enables AI assistants like Claude Desktop and Claude Code to dynamically interact with your development environment.

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Setup and Configuration](#setup-and-configuration)
4. [Available MCP Tools](#available-mcp-tools)
5. [Output Routing System](#output-routing-system)
6. [Practical Examples](#practical-examples)
7. [Integration with Pipelines](#integration-with-pipelines)
8. [Troubleshooting](#troubleshooting)
9. [Best Practices](#best-practices)

## Overview

Cafedelic's pane management system provides:

- **Dynamic Pane Naming**: Assign meaningful names to tmux panes instead of using coordinates
- **Output Routing**: Configure where different types of output (files, logs, errors) should go
- **MCP Integration**: AI assistants can interact with panes through standardized tools
- **Flexible Control**: Change routing on-the-fly without modifying code

### Key Benefits

- No hard-coded pane destinations in scripts
- AI assistants can read from and write to specific panes
- Different output types can be routed to dedicated panes
- Easy reconfiguration without code changes

## Architecture

The system consists of several components:

```
┌─────────────────────┐     ┌─────────────────────┐
│   Claude Desktop    │     │    Claude Code      │
│  (uses stdio MCP)   │     │   (uses MCP tools)  │
└──────────┬──────────┘     └──────────┬──────────┘
           │                           │
           ▼                           ▼
┌─────────────────────────────────────────────────┐
│              MCP Server Layer                    │
│  ┌─────────────────┐    ┌───────────────────┐  │
│  │ mcp-stdio.js    │    │ src/mcp-server.ts │  │
│  │ (stdio server)  │    │ (HTTP server)     │  │
│  └────────┬────────┘    └─────────┬─────────┘  │
│           └────────────┬───────────┘            │
│                        ▼                        │
│              ┌─────────────────┐               │
│              │  pane-tools.ts  │               │
│              │ (tool handlers) │               │
│              └────────┬────────┘               │
└───────────────────────┼─────────────────────────┘
                        ▼
        ┌───────────────────────────────┐
        │    Shell Scripts Layer        │
        │  scripts/pane-management/     │
        │  scripts/routing/             │
        └───────────────┬───────────────┘
                        ▼
        ┌───────────────────────────────┐
        │      Tmux Panes               │
        │  (your actual terminals)      │
        └───────────────────────────────┘
```

### Storage Locations

Configuration files are stored in `~/.cafedelic/`:

- **`pane-names`**: Maps custom names to pane coordinates
- **`routing-config`**: Maps output types to pane names

## Setup and Configuration

### 1. Starting the MCP Server

For Claude Desktop integration (stdio mode):
```bash
# Start the stdio server (usually configured in Claude Desktop)
node mcp-stdio.js
```

For HTTP API access:
```bash
# Start the HTTP server on port 3001
npm run start
```

### 2. Initial Pane Setup

First, create your tmux layout:
```bash
# Create a new tmux session
tmux new-session -s dev

# Split into panes as needed
# Ctrl-B % (vertical split)
# Ctrl-B " (horizontal split)
```

### 3. Naming Your Panes

Use the MCP tools to assign names to panes. From Claude Desktop or through the API:

```javascript
// Example: Name the editor pane
await mcp.callTool('assign_name_to_pane', {
  session: 'dev',
  window: 0,
  pane: 0,
  name: 'editor'
});

// Name other panes
await mcp.callTool('assign_name_to_pane', {
  session: 'dev',
  window: 0,
  pane: 1,
  name: 'terminal'
});

await mcp.callTool('assign_name_to_pane', {
  session: 'dev',
  window: 0,
  pane: 2,
  name: 'logs'
});
```

### 4. Configure Output Routing

Set up where different output types should go:

```javascript
// Route file operations to editor pane
await mcp.callTool('set_output_destination', {
  type: 'files',
  pane: 'editor'
});

// Route logs to logs pane
await mcp.callTool('set_output_destination', {
  type: 'logs',
  pane: 'logs'
});

// Route terminal commands to terminal pane
await mcp.callTool('set_output_destination', {
  type: 'terminal',
  pane: 'terminal'
});
```

## Available MCP Tools

### Pane Naming and Management

#### `assign_name_to_pane`
Assigns a custom name to a tmux pane.

**Parameters:**
- `session`: Tmux session name
- `window`: Window name or index
- `pane`: Pane index (0-based)
- `name`: Custom name (no spaces, colons, or dots)

**Example:**
```javascript
{
  "session": "dev",
  "window": 0,
  "pane": 1,
  "name": "editor"
}
```

#### `list_named_panes`
Lists all panes with assigned names, showing their status and running commands.

**Returns:**
```
Named Panes:
- editor: dev:0.0 (active) - Running: nvim
- terminal: dev:0.1 - Running: bash
- logs: dev:0.2 - Running: tail -f app.log
```

#### `unname_pane`
Removes a custom name from a pane.

**Parameters:**
- `name`: The custom name to remove

### Pane Interaction

#### `read_pane_by_name`
Reads the last N lines from a named pane.

**Parameters:**
- `name`: Pane name
- `lines`: Number of lines to read (default: 100)

**Example:**
```javascript
{
  "name": "logs",
  "lines": 50
}
```

#### `send_keys_to_pane`
Sends keys/text to a named pane.

**Parameters:**
- `name`: Pane name
- `text`: Keys/text to send

**Example:**
```javascript
{
  "name": "terminal",
  "text": "npm run test"
}
```

#### `send_special_key_to_pane`
Sends special keys to a pane.

**Parameters:**
- `name`: Pane name
- `key`: Special key (enter, escape, tab, ctrl-c, etc.)

**Supported Keys:**
- enter, escape, tab
- ctrl-c, ctrl-d, ctrl-z
- up, down, left, right
- home, end, page-up, page-down

#### `send_ctrl_c_to_pane_by_name`
Specialized Ctrl-C sender with double-tap option.

**Parameters:**
- `name`: Pane name
- `double_tap`: Send Ctrl-C twice (for exiting certain apps)

#### `get_details_for_pane_by_name`
Gets detailed information about a named pane.

**Returns:**
```javascript
{
  "name": "editor",
  "coordinates": "dev:0.0",
  "command": "nvim",
  "dimensions": "80x24",
  "active": true
}
```

### Routing Configuration

#### `set_output_destination`
Configures where a specific output type should be routed.

**Parameters:**
- `type`: Output type (files, activity, logs, errors, terminal)
- `pane`: Target pane name

**Output Types:**
- `files`: File operations (create, edit, delete)
- `activity`: General activity updates
- `logs`: Application logs
- `errors`: Error messages
- `terminal`: Terminal commands and output

#### `get_routing_config`
Displays the current routing configuration.

**Returns:**
```
Current Routing Configuration:
- files → editor
- activity → terminal
- logs → logs
- errors → logs
- terminal → terminal
```

## Output Routing System

The routing system allows dynamic configuration of where different types of output should go. This is particularly useful when integrating with AI assistants that need to send different types of information to appropriate panes.

### How It Works

1. **Output Classification**: Operations are classified into types (files, logs, errors, etc.)
2. **Route Lookup**: The system checks the routing configuration for the output type
3. **Pane Resolution**: The pane name is resolved to actual tmux coordinates
4. **Delivery**: Output is sent to the appropriate pane

### Configuration File Format

The routing configuration (`~/.cafedelic/routing-config`) uses a simple format:
```
files=editor
logs=logs
errors=logs
terminal=terminal
activity=terminal
```

The pane names file (`~/.cafedelic/pane-names`) maps names to coordinates:
```
dev:0.0=editor
dev:0.1=terminal
dev:0.2=logs
```

## Practical Examples

### Example 1: Development Environment Setup

```javascript
// 1. Create a three-pane layout (already done in tmux)
// 2. Name the panes
await mcp.callTool('assign_name_to_pane', {
  session: 'dev', window: 0, pane: 0, name: 'editor'
});
await mcp.callTool('assign_name_to_pane', {
  session: 'dev', window: 0, pane: 1, name: 'terminal'
});
await mcp.callTool('assign_name_to_pane', {
  session: 'dev', window: 0, pane: 2, name: 'logs'
});

// 3. Configure routing
await mcp.callTool('set_output_destination', {
  type: 'files', pane: 'editor'
});
await mcp.callTool('set_output_destination', {
  type: 'terminal', pane: 'terminal'
});
await mcp.callTool('set_output_destination', {
  type: 'logs', pane: 'logs'
});
await mcp.callTool('set_output_destination', {
  type: 'errors', pane: 'logs'
});

// 4. Verify configuration
const config = await mcp.callTool('get_routing_config', {});
console.log(config);
```

### Example 2: Running Tests and Monitoring Output

```javascript
// Send test command to terminal
await mcp.callTool('send_keys_to_pane', {
  name: 'terminal',
  text: 'npm test'
});
await mcp.callTool('send_special_key_to_pane', {
  name: 'terminal',
  key: 'enter'
});

// Read test output
const output = await mcp.callTool('read_pane_by_name', {
  name: 'terminal',
  lines: 50
});

// If tests are hanging, send Ctrl-C
await mcp.callTool('send_ctrl_c_to_pane_by_name', {
  name: 'terminal',
  double_tap: false
});
```

### Example 3: Monitoring Logs

```javascript
// Start log tailing in logs pane
await mcp.callTool('send_keys_to_pane', {
  name: 'logs',
  text: 'tail -f /var/log/app.log'
});
await mcp.callTool('send_special_key_to_pane', {
  name: 'logs',
  key: 'enter'
});

// Periodically read logs
const logs = await mcp.callTool('read_pane_by_name', {
  name: 'logs',
  lines: 100
});
```

## Integration with Pipelines

Cafedelic's Watch-Transform-Execute (WTE) pipelines can use the routing system to dynamically determine output destinations.

### Using Routing in Custom Pipelines

```typescript
import { loadRoutingConfig, getTargetPane } from './src/routing/config';

// In your pipeline
async function routeOutput(type: string, content: string) {
  const config = await loadRoutingConfig();
  const targetPane = await getTargetPane(type);
  
  if (targetPane) {
    // Send to the configured pane
    await sendToPane(targetPane.name, content);
  }
}
```

### Creating Routing-Aware Transforms

```typescript
import { createRoutingTransform } from './src/routing/config';

// Create a transform that respects routing configuration
const transform = createRoutingTransform('files', async (input) => {
  // Your transformation logic
  return transformedOutput;
});
```

## Troubleshooting

### Common Issues

#### 1. "Pane not found" Error
- **Cause**: The pane no longer exists but the name mapping remains
- **Solution**: List panes to identify stale entries, then unname them
```javascript
await mcp.callTool('list_named_panes', {});
await mcp.callTool('unname_pane', { name: 'stale-pane' });
```

#### 2. "Invalid pane name" Error
- **Cause**: Name contains forbidden characters (spaces, colons, dots)
- **Solution**: Use only alphanumeric characters, hyphens, and underscores

#### 3. Output Going to Wrong Pane
- **Cause**: Routing configuration is incorrect or outdated
- **Solution**: Check and update routing configuration
```javascript
const config = await mcp.callTool('get_routing_config', {});
// Update as needed
await mcp.callTool('set_output_destination', {
  type: 'files',
  pane: 'correct-pane'
});
```

### Debugging Commands

Check pane existence:
```bash
tmux list-panes -t dev -F "#{pane_index}: #{pane_current_command}"
```

View configuration files:
```bash
cat ~/.cafedelic/pane-names
cat ~/.cafedelic/routing-config
```

Clear all configurations:
```bash
rm ~/.cafedelic/pane-names
rm ~/.cafedelic/routing-config
```

## Best Practices

### 1. Naming Conventions
- Use descriptive names: `editor`, `tests`, `server-logs`
- Avoid generic names: `pane1`, `temp`, `misc`
- Group related panes: `backend-logs`, `backend-terminal`

### 2. Routing Strategy
- Keep similar outputs together (logs + errors)
- Separate interactive and non-interactive panes
- Consider workflow when arranging panes

### 3. Session Management
- Use consistent session names across projects
- Document your standard layouts
- Create setup scripts for common configurations

### 4. Error Handling
- Always check if panes exist before sending commands
- Handle routing failures gracefully
- Provide fallback destinations

### 5. Security Considerations
- Validate all input to prevent injection
- Use the built-in validation in MCP tools
- Don't expose MCP server to untrusted networks

## Advanced Usage

### Dynamic Routing Based on Context

```javascript
// Route errors to different panes based on severity
async function routeError(error, severity) {
  const pane = severity === 'critical' ? 'alerts' : 'logs';
  await mcp.callTool('send_keys_to_pane', {
    name: pane,
    text: `[${severity.toUpperCase()}] ${error.message}`
  });
}
```

### Automated Layout Setup

Create a script to set up your standard layout:

```javascript
async function setupDevEnvironment() {
  const panes = [
    { index: 0, name: 'editor', routing: ['files'] },
    { index: 1, name: 'terminal', routing: ['terminal', 'activity'] },
    { index: 2, name: 'logs', routing: ['logs', 'errors'] },
    { index: 3, name: 'tests', routing: [] }
  ];
  
  for (const pane of panes) {
    // Name the pane
    await mcp.callTool('assign_name_to_pane', {
      session: 'dev',
      window: 0,
      pane: pane.index,
      name: pane.name
    });
    
    // Set up routing
    for (const type of pane.routing) {
      await mcp.callTool('set_output_destination', {
        type: type,
        pane: pane.name
      });
    }
  }
}
```

### Monitoring and Alerts

```javascript
// Monitor a pane for specific patterns
async function monitorPane(paneName, pattern, callback) {
  setInterval(async () => {
    const content = await mcp.callTool('read_pane_by_name', {
      name: paneName,
      lines: 50
    });
    
    if (content.includes(pattern)) {
      callback(content);
    }
  }, 5000);
}

// Example: Alert on errors
monitorPane('logs', 'ERROR', async (content) => {
  await mcp.callTool('send_keys_to_pane', {
    name: 'alerts',
    text: '🚨 Error detected in logs!'
  });
});
```

## Conclusion

Cafedelic's MCP-based tmux pane management system provides a powerful and flexible way to manage output routing in development environments. By naming panes and configuring routing rules, you can create sophisticated workflows that adapt to your needs without modifying code.

The integration with AI assistants through MCP makes it possible to build intelligent automation that respects your preferred working environment while maintaining full control over where different types of output appear.

For more information, see:
- [Pane Tools Documentation](./pane-tools.md)
- [MCP Server Implementation](../src/mcp-tools/)
- [Example Scripts](../scripts/pane-management/)