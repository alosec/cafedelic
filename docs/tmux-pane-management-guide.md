# Tmux Pane Management Guide for Cafedelic MCP Server

This guide explains how to use Cafedelic's MCP (Model Context Protocol) server to manage tmux panes through a property-based system. The system enables AI assistants like Claude Desktop and Claude Code to dynamically interact with your development environment.

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Setup and Configuration](#setup-and-configuration)
4. [Available MCP Tools](#available-mcp-tools)
5. [Property-Based Pane Management](#property-based-pane-management)
6. [Practical Examples](#practical-examples)
7. [Troubleshooting](#troubleshooting)
8. [Best Practices](#best-practices)

## Overview

Cafedelic's pane management system provides:

- **Dynamic Pane Naming**: Assign meaningful names to tmux panes instead of using coordinates
- **Property-Based Discovery**: Find panes by their semantic properties (source, role)
- **MCP Integration**: AI assistants can interact with panes through standardized tools
- **Flexible Control**: Assign and query pane properties dynamically

### Key Benefits

- No hard-coded pane destinations in scripts
- AI assistants can read from and write to specific panes
- Semantic pane discovery based on properties
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
        │  scripts/pane-properties/     │
        └───────────────┬───────────────┘
                        ▼
        ┌───────────────────────────────┐
        │      Tmux Panes               │
        │  (your actual terminals)      │
        └───────────────────────────────┘
```

### Storage Locations

Configuration files are stored in `~/.cafedelic/`:

- **`pane-names`**: Maps custom names to pane coordinates (for backward compatibility)

Pane properties are stored directly in tmux as user options:
- `@pane_name`: Custom name for the pane
- `@source`: Source context (user, claude-desktop, claude-code, system)
- `@role`: Semantic role (editor, terminal, logs, tests, debug, monitor)

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

### 3. Assigning Properties to Your Panes

Use the MCP tools to assign properties to panes. From Claude Desktop or through the API:

```javascript
// Example: Assign properties to the editor pane
await mcp.callTool('assign_pane_properties', {
  session: 'dev',
  window: 0,
  pane: 0,
  name: 'editor',
  source: 'user',
  role: 'editor'
});

// Assign properties to other panes
await mcp.callTool('assign_pane_properties', {
  session: 'dev',
  window: 0,
  pane: 1,
  name: 'terminal',
  source: 'user',
  role: 'terminal'
});

await mcp.callTool('assign_pane_properties', {
  session: 'dev',
  window: 0,
  pane: 2,
  name: 'logs',
  source: 'system',
  role: 'logs'
});
```

## Available MCP Tools

### Pane Naming and Management

#### `assign_name_to_pane`
Assigns a custom name to a tmux pane (backward compatibility).

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

#### `assign_pane_properties`
Assigns properties to a tmux pane (recommended approach).

**Parameters:**
- `session`: Tmux session name
- `window`: Window name or index
- `pane`: Pane index (0-based)
- `name`: Custom name (optional)
- `source`: Source context (optional: user, claude-desktop, claude-code, system)
- `role`: Semantic role (optional: editor, terminal, logs, tests, debug, monitor)

**Example:**
```javascript
{
  "session": "dev",
  "window": 0,
  "pane": 1,
  "name": "main-editor",
  "source": "user",
  "role": "editor"
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

#### `list_panes_by_properties`
Lists panes filtered by properties.

**Parameters:**
- `source`: Filter by source (optional)
- `role`: Filter by role (optional)
- `name`: Filter by name (optional)

**Example:**
```javascript
// Find all editor panes
{
  "role": "editor"
}

// Find all panes from claude-code
{
  "source": "claude-code"
}
```

#### `find_pane_by_source_and_role`
Finds a specific pane by source and role combination.

**Parameters:**
- `source`: Source to find
- `role`: Role to find

**Example:**
```javascript
{
  "source": "user",
  "role": "terminal"
}
```

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
Sends text to a named pane.

**Parameters:**
- `name`: Pane name
- `text`: Text to send

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

## Property-Based Pane Management

The property system allows semantic identification of panes based on their purpose and origin.

### Properties

1. **name**: A custom identifier for the pane
   - No spaces, colons, or dots allowed
   - Must be unique across all panes

2. **source**: Indicates what created or owns the pane
   - `user`: Created by the user manually
   - `claude-desktop`: Created for Claude Desktop
   - `claude-code`: Created for Claude Code
   - `system`: System-level panes (logs, monitoring)

3. **role**: The semantic purpose of the pane
   - `editor`: Code editing
   - `terminal`: Command execution
   - `logs`: Log viewing
   - `tests`: Test execution
   - `debug`: Debugging sessions
   - `monitor`: System monitoring

### Discovery Patterns

Find panes using property combinations:

```javascript
// Find the user's terminal
const pane = await mcp.callTool('find_pane_by_source_and_role', {
  source: 'user',
  role: 'terminal'
});

// List all editor panes
const editors = await mcp.callTool('list_panes_by_properties', {
  role: 'editor'
});

// Find Claude Code's output
const codeOutput = await mcp.callTool('find_pane_by_source_and_role', {
  source: 'claude-code',
  role: 'terminal'
});
```

## Practical Examples

### Example 1: Development Environment Setup

```javascript
// 1. Create a three-pane layout (already done in tmux)
// 2. Assign properties to panes
await mcp.callTool('assign_pane_properties', {
  session: 'dev', 
  window: 0, 
  pane: 0, 
  name: 'editor',
  source: 'user',
  role: 'editor'
});

await mcp.callTool('assign_pane_properties', {
  session: 'dev', 
  window: 0, 
  pane: 1, 
  name: 'terminal',
  source: 'user',
  role: 'terminal'
});

await mcp.callTool('assign_pane_properties', {
  session: 'dev', 
  window: 0, 
  pane: 2, 
  name: 'logs',
  source: 'system',
  role: 'logs'
});

// 3. Verify setup
const panes = await mcp.callTool('list_panes_by_properties', {});
console.log(panes);
```

### Example 2: Running Tests and Monitoring Output

```javascript
// Find the user's terminal
const terminalPane = await mcp.callTool('find_pane_by_source_and_role', {
  source: 'user',
  role: 'terminal'
});

// Send test command
await mcp.callTool('send_keys_to_pane', {
  name: terminalPane.name,
  text: 'npm test'
});
await mcp.callTool('send_special_key_to_pane', {
  name: terminalPane.name,
  key: 'enter'
});

// Read test output
const output = await mcp.callTool('read_pane_by_name', {
  name: terminalPane.name,
  lines: 50
});

// If tests are hanging, send Ctrl-C
await mcp.callTool('send_ctrl_c_to_pane_by_name', {
  name: terminalPane.name,
  double_tap: false
});
```

### Example 3: Monitoring System Logs

```javascript
// Find or create a logs pane
const logPane = await mcp.callTool('find_pane_by_source_and_role', {
  source: 'system',
  role: 'logs'
});

// Start log tailing
await mcp.callTool('send_keys_to_pane', {
  name: logPane.name,
  text: 'tail -f /var/log/app.log'
});
await mcp.callTool('send_special_key_to_pane', {
  name: logPane.name,
  key: 'enter'
});

// Periodically read logs
const logs = await mcp.callTool('read_pane_by_name', {
  name: logPane.name,
  lines: 100
});
```

## Troubleshooting

### Common Issues

#### 1. "Pane not found" Error
- **Cause**: The pane no longer exists but the name mapping remains
- **Solution**: List panes to identify stale entries
```javascript
await mcp.callTool('list_named_panes', {});
await mcp.callTool('list_panes_by_properties', {});
```

#### 2. "Invalid pane name" Error
- **Cause**: Name contains forbidden characters (spaces, colons, dots)
- **Solution**: Use only alphanumeric characters, hyphens, and underscores

#### 3. Cannot Find Pane by Properties
- **Cause**: Properties not set or pane doesn't exist
- **Solution**: List all panes with properties to verify
```javascript
const allPanes = await mcp.callTool('list_panes_by_properties', {});
```

### Debugging Commands

Check pane existence:
```bash
tmux list-panes -t dev -F "#{pane_index}: #{pane_current_command}"
```

View pane properties:
```bash
# Check a specific pane's properties
tmux show-options -p -t dev:0.0
```

View configuration files:
```bash
cat ~/.cafedelic/pane-names
```

## Best Practices

### 1. Naming Conventions
- Use descriptive names: `editor`, `tests`, `server-logs`
- Avoid generic names: `pane1`, `temp`, `misc`
- Group related panes: `backend-logs`, `backend-terminal`

### 2. Property Strategy
- Always set both source and role for better discovery
- Use consistent source values across your workflow
- Choose appropriate roles that reflect pane purpose

### 3. Session Management
- Use consistent session names across projects
- Document your standard layouts
- Create setup scripts for common configurations

### 4. Error Handling
- Always check if panes exist before sending commands
- Handle discovery failures gracefully
- Provide fallback strategies

### 5. Security Considerations
- Validate all input to prevent injection
- Use the built-in validation in MCP tools
- Don't expose MCP server to untrusted networks

## Advanced Usage

### Dynamic Pane Discovery

```javascript
// Find any available terminal pane
async function findTerminal() {
  // Try user terminal first
  try {
    return await mcp.callTool('find_pane_by_source_and_role', {
      source: 'user',
      role: 'terminal'
    });
  } catch (e) {
    // Fall back to any terminal
    const terminals = await mcp.callTool('list_panes_by_properties', {
      role: 'terminal'
    });
    if (terminals.length > 0) {
      return terminals[0];
    }
    throw new Error('No terminal pane available');
  }
}
```

### Automated Layout Setup

Create a script to set up your standard layout:

```javascript
async function setupDevEnvironment() {
  const panes = [
    { index: 0, name: 'editor', source: 'user', role: 'editor' },
    { index: 1, name: 'terminal', source: 'user', role: 'terminal' },
    { index: 2, name: 'logs', source: 'system', role: 'logs' },
    { index: 3, name: 'tests', source: 'user', role: 'tests' }
  ];
  
  for (const pane of panes) {
    await mcp.callTool('assign_pane_properties', {
      session: 'dev',
      window: 0,
      pane: pane.index,
      name: pane.name,
      source: pane.source,
      role: pane.role
    });
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
const logPane = await mcp.callTool('find_pane_by_source_and_role', {
  source: 'system',
  role: 'logs'
});

monitorPane(logPane.name, 'ERROR', async (content) => {
  const alertPane = await mcp.callTool('find_pane_by_source_and_role', {
    source: 'system',
    role: 'monitor'
  });
  
  await mcp.callTool('send_keys_to_pane', {
    name: alertPane.name,
    text: '🚨 Error detected in logs!'
  });
});
```

## Conclusion

Cafedelic's MCP-based tmux pane management system provides a powerful and flexible way to manage panes in development environments. By using properties to semantically identify panes, you can create sophisticated workflows that adapt to your needs without modifying code.

The integration with AI assistants through MCP makes it possible to build intelligent automation that respects your preferred working environment while maintaining full control over pane interactions.

For more information, see:
- [Pane Tools Documentation](./pane-tools.md)
- [MCP Server Implementation](../src/mcp-tools/)
- [Example Scripts](../scripts/pane-management/)
