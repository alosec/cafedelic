# Tmux Property-Based Pane Management Guide for Cafedelic MCP Server

This guide explains how to use Cafedelic's MCP (Model Context Protocol) server to manage tmux panes using a property-based system. The system enables AI assistants like Claude Desktop and Claude Code to dynamically interact with your development environment using semantic properties instead of brittle coordinates.

## Table of Contents

1. [Overview](#overview)
2. [Property-Based Architecture](#property-based-architecture)
3. [Available MCP Tools](#available-mcp-tools)
4. [Setup and Configuration](#setup-and-configuration)
5. [Practical Examples](#practical-examples)
6. [Migration from Name-Based System](#migration-from-name-based-system)
7. [Troubleshooting](#troubleshooting)
8. [Best Practices](#best-practices)

## Overview

Cafedelic's property-based pane management system provides:

- **Semantic Identification**: Identify panes by purpose (role) and context (source)
- **Rich Content Capture**: Advanced tmux capture capabilities with search and formatting
- **MCP Integration**: AI assistants can interact with panes through standardized tools
- **Flexible Discovery**: Find panes by multiple properties simultaneously

### Key Benefits

- No coordinate-based pane references that break when layouts change
- AI assistants can intelligently route output based on context
- Rich capture capabilities expose full tmux power
- Property-based filtering for complex multi-pane setups

## Property-Based Architecture

The system uses two primary properties to identify panes:

### Source Properties
Indicates who or what controls the pane:
- `user` - User-controlled terminal sessions
- `claude-desktop` - Panes managed by Claude Desktop
- `claude-code` - Panes managed by Claude Code
- `system` - System monitoring, logs, or automated processes

### Role Properties  
Indicates the semantic purpose of the pane:
- `editor` - Text editing, code editing
- `terminal` - Interactive command execution
- `logs` - Log monitoring, tailing files
- `tests` - Running tests, test output
- `debug` - Debugging sessions, debugger output
- `monitor` - System monitoring, process watching

### Property Combinations
Examples of meaningful combinations:
- `source=claude-desktop, role=editor` - Where Claude Desktop opens files
- `source=user, role=terminal` - User's main command interface  
- `source=system, role=logs` - System log monitoring
- `source=claude-code, role=tests` - Claude Code test execution

## Available MCP Tools

### Pane Property Management

#### `assign_pane_properties`
Assigns properties to a tmux pane using coordinates.

**Parameters:**
- `session`: Tmux session name (required)
- `window`: Window name or index (required)  
- `pane`: Pane index 0-based (required)
- `name`: Custom name (optional, no spaces/colons/dots)
- `source`: Source context (optional)
- `role`: Semantic role (optional)

**Example:**
```javascript
{
  "session": "dev",
  "window": 0,
  "pane": 1,
  "name": "main-editor",
  "source": "claude-desktop", 
  "role": "editor"
}
```

#### `list_panes_by_properties`
Lists all panes matching specified property filters.

**Parameters (all optional):**
- `source`: Filter by source
- `role`: Filter by role  
- `name`: Filter by name

**Example:**
```javascript
{
  "source": "claude-desktop",
  "role": "editor"
}
```

**Returns:**
```
Panes with properties:
=====================

Pane: dev:0.1
  name: main-editor
  source: claude-desktop
  role: editor
  command: nvim
```

#### `find_pane_by_source_and_role`  
Finds exactly one pane matching source and role.

**Parameters:**
- `source`: Source to find (required)
- `role`: Role to find (required)

**Example:**
```javascript
{
  "source": "claude-desktop",
  "role": "editor"  
}
```

**Returns:** `dev:0.1` (pane coordinates) or error if not found/multiple matches

### Pane Content Capture

#### `capture_pane_with_properties`
Advanced pane content capture with tmux's full power. Replaces the deprecated `read_pane_by_name`.

**Property Selection (use one or more):**
- `source`: Source context filter
- `role`: Role filter
- `name`: Name filter

**Capture Range Options:**
- `last`: Number of last lines (shortcut, overrides start/end)
- `start`: Start line number or "-" for beginning  
- `end`: End line number or "-" for end

**Output Formatting:**
- `join_lines`: Join wrapped lines (boolean)
- `escape_sequences`: Preserve colors/formatting (boolean)
- `preserve_trailing`: Preserve trailing spaces (boolean)

**Search/Filter:**
- `grep`: Pattern to search for
- `grep_context`: Lines of context around matches (default: 0)
- `invert_match`: Show non-matching lines (boolean)

**Example - Get last 50 lines from Claude Desktop editor:**
```javascript
{
  "source": "claude-desktop",
  "role": "editor", 
  "last": 50
}
```

**Example - Search for errors in logs with context:**
```javascript
{
  "source": "system",
  "role": "logs",
  "grep": "ERROR",
  "grep_context": 3
}
```

### Pane Interaction

#### `send_keys_to_pane`
Sends text to a named pane.

**Parameters:**
- `name`: Pane name (required)
- `text`: Text to send (required)

#### `send_special_key_to_pane`
Sends special keys to a named pane.

**Parameters:**
- `name`: Pane name (required)  
- `key`: Special key (required)

**Supported Keys:**
- enter, escape, tab
- ctrl-c, ctrl-d, ctrl-z
- up, down, left, right  
- home, end, page-up, page-down

#### `send_ctrl_c_to_pane_by_name`
Specialized Ctrl-C sender with double-tap option.

**Parameters:**
- `name`: Pane name (required)
- `double_tap`: Send Ctrl-C twice for app exit (boolean, default: false)

## Setup and Configuration

### Property Assignment Workflow

The typical workflow for setting up property-based pane management:

1. **Create your tmux layout** (using tmux commands)
2. **Assign properties** to each pane using `assign_pane_properties`
3. **Verify setup** using `list_panes_by_properties`
4. **Use property-based discovery** in workflows

### Demo Environment Setup

**Session 4 with Claude Desktop integration:**

```javascript
// Pane 0: Raw Claude Desktop logs
await mcp.callTool('assign_pane_properties', {
  session: '4',
  window: 0,
  pane: 0,
  name: 'claude-logs',
  source: 'claude-desktop', 
  role: 'logs'
});

// Pane 1: Claude Desktop editor workspace  
await mcp.callTool('assign_pane_properties', {
  session: '4',
  window: 0,
  pane: 1,
  name: 'editor',
  source: 'claude-desktop',
  role: 'editor'
});

// Pane 2: Processed/simplified logs
await mcp.callTool('assign_pane_properties', {
  session: '4',
  window: 0,
  pane: 2, 
  name: 'readable-logs',
  source: 'system',
  role: 'logs'
});

// Start log tailing in pane 0
await mcp.callTool('send_keys_to_pane', {
  name: 'claude-logs',
  text: 'tail -f ~/.config/Claude/logs/mcp-*.log'
});
```

### Development Environment Example

```javascript
// Standard 3-pane development layout
// Pane 0: Editor managed by Claude Desktop
await mcp.callTool('assign_pane_properties', {
  session: 'dev',
  window: 0,
  pane: 0,
  name: 'editor',
  source: 'claude-desktop',
  role: 'editor'
});

// Pane 1: User terminal
await mcp.callTool('assign_pane_properties', {
  session: 'dev', 
  window: 0,
  pane: 1,
  name: 'terminal',
  source: 'user',
  role: 'terminal'  
});

// Pane 2: System logs
await mcp.callTool('assign_pane_properties', {
  session: 'dev',
  window: 0, 
  pane: 2,
  name: 'logs',
  source: 'system',
  role: 'logs'
});
```

## Demo Verification

The demo environment is now live and functional:

```bash
# View current setup
$ tmux list-panes -t 4:0 -F "#{pane_index}: #{@source}/#{@role} (#{@pane_name})"
0: claude-desktop/logs (claude-logs)
1: claude-desktop/editor (editor)  
2: system/logs (readable-logs)

# Test property-based discovery
$ capture_pane_with_properties --source claude-desktop --role logs --last 5
# Shows real-time MCP log activity

$ find_pane_by_source_and_role claude-desktop editor
# Returns: 4:0.1
```

## Troubleshooting

### Common Issues

#### 1. "No pane found" Error
Use `list_panes_by_properties` to see what exists:
```javascript
await mcp.callTool('list_panes_by_properties', {});
```

#### 2. Properties not set
Verify using tmux directly:
```bash
tmux show-options -p -t "4:0.0" @source @role @pane_name
```

#### 3. Log tail not working
Check the log file exists:
```bash
ls -la ~/.config/Claude/logs/mcp-*.log
```

## Best Practices

### 1. Property Assignment Strategy
- Use consistent source values across projects
- Map roles to actual pane purposes  
- Avoid duplicate source+role pairs per session

### 2. Discovery Patterns
- Use `find_pane_by_source_and_role` for specific targeting
- Use `list_panes_by_properties` for exploration
- Use `capture_pane_with_properties` with filters for content

### 3. Error Handling
- Always check if panes exist before operations
- Handle missing panes gracefully in scripts
- Verify properties are set after assignment

For complete implementation details, see the MCP server code in `mcp-stdio.js` and the shell scripts in `scripts/pane-properties/`.
