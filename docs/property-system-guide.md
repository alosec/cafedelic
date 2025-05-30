# Using the Multi-Dimensional Pane Property System

## Quick Start

### 1. Set up your panes with properties

```bash
# For Claude Desktop's editor pane
assign_pane_properties({
  session: "dev",
  window: 0,
  pane: 0,
  name: "claude-editor",     # backward compatible name
  source: "claude-desktop",   # NEW: source context
  role: "editor"             # NEW: semantic role
})

# For your personal editor pane
assign_pane_properties({
  session: "dev", 
  window: 0,
  pane: 1,
  name: "my-editor",
  source: "user",
  role: "editor"
})

# For shared logs
assign_pane_properties({
  session: "dev",
  window: 0, 
  pane: 2,
  name: "logs",
  source: "system",
  role: "logs"
})
```

### 2. Query panes by properties

```bash
# Find Claude's editor
find_pane_by_source_and_role("claude-desktop", "editor")
# Returns: dev:0.0

# List all Claude Desktop panes
list_panes_by_properties({ source: "claude-desktop" })

# List all editor panes regardless of source
list_panes_by_properties({ role: "editor" })
```

### 3. Use in WTE pipelines

```typescript
import { propertyBasedEmacsExecutor } from './executors/property-based-emacs.js';

// Claude Desktop file operations go to Claude's editor
pipe(
  mcpLogWatcher(),
  fileOperationTransform,
  propertyBasedEmacsExecutor({
    defaultSource: 'claude-desktop',
    defaultRole: 'editor'
  })
);
```

## Natural Language Examples

With Claude, you can now say:

- "Make this pane my Claude Desktop editor"
- "Set up pane 2 as the system logs viewer"
- "Route Claude Code output to its own terminal"
- "Show me all panes assigned to Claude Desktop"

## Migration from Name-Only System

Existing setups continue to work! The internal routing system provides smart fallbacks:

1. First tries: exact source + role match
2. Then tries: any pane with the role  
3. Finally tries: pane with @name matching the role

So if you have a pane named "editor", it will be found even without properties.

## Benefits Over Coordinate-Based System

Before:
```bash
export CAFEDELIC_EDITOR_PANE="0:1.2"  # Breaks when layout changes!
```

After:
```bash
# Just works, even after layout changes
# Internal routing finds the right pane automatically
find_pane_by_source_and_role("claude-desktop", "editor")
```

No more:
- Hardcoded pane coordinates
- Environment variable management
- Broken pipelines after tmux reorganization
- Manual coordinate lookups

## Advanced Usage

### Conditional Routing
```typescript
// Route based on context
const executor = async (op) => {
  const source = detectSource(op);  // Your logic here
  const pane = await findBestPane('editor', source);
  await executeInPane(pane, op);
};
```

### Multi-Agent Setup
```bash
# Claude Desktop gets its own workspace
assign_pane_properties({ ..., source: "claude-desktop", role: "editor" })
assign_pane_properties({ ..., source: "claude-desktop", role: "terminal" })

# Claude Code gets separate panes
assign_pane_properties({ ..., source: "claude-code", role: "editor" })
assign_pane_properties({ ..., source: "claude-code", role: "terminal" })

# Your panes remain separate
assign_pane_properties({ ..., source: "user", role: "editor" })
```

The system ensures each agent's outputs go to the right places automatically!
