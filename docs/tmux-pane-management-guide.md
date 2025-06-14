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
```

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

## Available MCP Tools

### Pane Property Management

#### `assign_pane_properties`
Assigns properties to a tmux pane - the primary tool for pane management.

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

### Property Discovery Tools

#### `list_panes_by_properties`
Filter and list panes by their assigned properties.

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
Find a specific pane using source and role combination.

**Parameters:**
- `source`: Source to find (required)
- `role`: Role to find (required)

**Example:**
```javascript
{
  "source": "user",
  "role": "terminal"
}
```

#### `list_named_panes`
List all panes that have been assigned names or properties.

**Returns:**
```
Named Panes:
- editor: dev:0.0 (active) - Running: nvim
- terminal: dev:0.1 - Running: bash
- logs: dev:0.2 - Running: tail -f app.log
```

### Pane Interaction

#### `read_pane_by_name`
Read the last N lines from a named pane.

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
Send text to a named pane.

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
Send special keys to a pane.

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

### Migration from Legacy Tools

The `assign_name_to_pane` tool has been **deprecated** in favor of the more powerful `assign_pane_properties` tool. The property-based approach provides:

- **Multi-dimensional identification**: Use source and role together
- **Better fallback logic**: Find appropriate panes when specific ones don't exist  
- **Future compatibility**: Supports complex development workflows

**Migration Example:**

```javascript
// OLD (deprecated - no longer available)
await mcp.callTool('assign_name_to_pane', {
  session: 'dev', window: 0, pane: 1, name: 'editor'
});

// NEW (recommended)
await mcp.callTool('assign_pane_properties', {
  session: 'dev', window: 0, pane: 1, 
  name: 'editor',
  source: 'user',
  role: 'editor'
});
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
```

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

## Troubleshooting

### Common Issues

#### 1. "Pane not found" Error
- **Cause**: The pane no longer exists but properties remain
- **Solution**: List panes to identify stale entries, then clean up properties
```javascript
await mcp.callTool('list_panes_by_properties', {});
```

#### 2. "Invalid pane properties" Error
- **Cause**: Properties contain forbidden characters or invalid values
- **Solution**: Use only valid values for source and role properties

#### 3. No pane found with properties
- **Cause**: No pane matches the requested source/role combination
- **Solution**: Check and update pane properties or use fallback logic
```javascript
const config = await mcp.callTool('list_panes_by_properties', {
  source: 'user'
});
// Update as needed
await mcp.callTool('assign_pane_properties', {
  session: 'dev', window: 0, pane: 1,
  source: 'user',
  role: 'editor'
});
```

### Debugging Commands

Check pane existence:
```bash
tmux list-panes -t dev -F "#{pane_index}: #{pane_current_command}"
```

View pane properties:
```bash
tmux show-options -p @pane_name @source @role
```

Clear pane properties:
```bash
tmux set-option -p @pane_name ""
tmux set-option -p @source ""
tmux set-option -p @role ""
```

## Best Practices

### 1. Property Assignment Conventions
- Use descriptive names: `main-editor`, `test-runner`, `server-logs`
- Assign meaningful sources: `user` for manual panes, `claude-desktop` for AI-managed
- Use consistent roles across projects

### 2. Multi-Assistant Workflows
- Separate panes by source for different AI assistants
- Use role-based discovery for flexible routing
- Consider workflow when arranging panes

### 3. Session Management
- Use consistent session names across projects
- Document your standard property assignments
- Create setup scripts for common configurations

### 4. Error Handling
- Always check if panes exist before sending commands
- Handle discovery failures gracefully
- Provide fallback destinations

### 5. Performance Considerations
- Property lookup is fast (tmux built-in)
- Batch property assignments when possible
- Use specific queries over broad searches

## Advanced Usage

### Dynamic Property Assignment

```javascript
// Assign properties based on context
async function setupDevEnvironment(aiSource = 'user') {
  const panes = [
    { index: 0, name: 'editor', role: 'editor' },
    { index: 1, name: 'terminal', role: 'terminal' },
    { index: 2, name: 'logs', role: 'logs' }
  ];
  
  for (const pane of panes) {
    await mcp.callTool('assign_pane_properties', {
      session: 'dev',
      window: 0,
      pane: pane.index,
      name: pane.name,
      source: aiSource,
      role: pane.role
    });
  }
}
```

### Smart Pane Discovery

```javascript
// Find best available pane with fallback
async function findBestEditorPane(preferredSource = 'user') {
  // Try exact match first
  try {
    return await mcp.callTool('find_pane_by_source_and_role', {
      source: preferredSource,
      role: 'editor'
    });
  } catch {
    // Fall back to any editor pane
    const editors = await mcp.callTool('list_panes_by_properties', {
      role: 'editor'
    });
    return editors[0]; // Return first available
  }
}
```

### Multi-AI Coordination

```javascript
// Set up separate environments for different AI assistants
await setupDevEnvironment('claude-desktop');
await setupDevEnvironment('claude-code');

// Each AI now has dedicated panes:
// claude-desktop: editor, terminal, logs
// claude-code: editor, terminal, logs
// user: can have separate panes as well
```

## Conclusion

Cafedelic's property-based MCP tmux pane management system provides a powerful and flexible way to manage development environments. By using semantic properties instead of hard-coded coordinates, you can create sophisticated workflows that adapt to your needs while maintaining clarity about which AI assistant or user context owns each pane.

The enhanced property system enables:
- **Clear ownership**: Know which AI assistant or user controls each pane
- **Semantic discovery**: Find panes by their purpose, not position
- **Flexible workflows**: Easy coordination between multiple AI assistants
- **Future-proof design**: Extensible property system for new use cases

The elimination of the deprecated `assign_name_to_pane` tool simplifies the API while providing more powerful functionality through the property-based approach.

For more information, see:
- [Pane Properties Scripts](../scripts/pane-properties/)
- [MCP Server Implementation](../src/mcp-tools/)
- [Integration Examples](../examples/)
