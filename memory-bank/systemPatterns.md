# System Patterns

## Core Architecture: Watch-Transform-Execute (WTE)

The entire cafedelic system is built on a simple, powerful pattern that composes three phases:

```typescript
interface WTE<W, T, E> {
  watch: () => AsyncIterator<W>      // Observe data sources
  transform: (data: W) => T | null   // Shape data for action
  execute: (action: T) => Promise<void> // Perform side effects
}
```

## Implementation Pattern

### The Pipe Utility
```typescript
// Functional composition for async iterators
export async function pipe<T, U>(
  source: AsyncIterator<T>,
  transform: (value: T) => U | null,
  execute: (value: U) => Promise<void>
): Promise<void> {
  for await (const value of source) {
    const transformed = transform(value);
    if (transformed !== null) {
      await execute(transformed);
    }
  }
}
```

### Real Example
```typescript
// Entire file-to-emacs pipeline
pipe(
  mcpLogWatcher('/home/alex/.config/Claude/logs/mcp-*.log'),
  fileOperationTransform,
  emacsExecutor('/path/to/open-file.sh')
);
```

## Design Principles

### 1. Direct Implementation
- No abstractions unless they simplify
- Code reads like what it does
- Functions over classes

### 2. Functional Composition
- Small, focused functions
- Compose behavior through piping
- Transform data, don't mutate state

### 3. Shell Script Integration
- Proven scripts do the real work
- TypeScript orchestrates and composes
- Clear boundary between coordination and execution

## Component Patterns

### Watchers
```typescript
// Watchers yield data from sources
async function* mcpLogWatcher(pattern: string) {
  const files = await glob(pattern);
  const latest = files[files.length - 1];
  
  const rl = readline.createInterface({
    input: fs.createReadStream(latest),
    crlfDelay: Infinity
  });
  
  for await (const line of rl) {
    yield line;
  }
}
```

### Transforms
```typescript
// Transforms shape data or return null to skip
function fileOperationTransform(line: string): FileOp | null {
  try {
    const entry = JSON.parse(line);
    if (entry.method === 'read_file') {
      return {
        type: 'file',
        path: entry.params.path
      };
    }
    return null;
  } catch {
    return null;
  }
}
```

### Executors
```typescript
// Executors perform side effects
function emacsExecutor(scriptPath: string) {
  return async (op: FileOp) => {
    await execAsync(`${scriptPath} "${op.path}"`);
  };
}
```

## Extension Patterns

### Adding Features
1. **New Data Source**: Create a watcher
2. **New Data Shape**: Create a transform
3. **New Side Effect**: Create an executor
4. **Combine**: Use `pipe()`

### Example: Git Monitoring
```typescript
// Watch git operations
async function* gitLogWatcher() { ... }

// Transform to git events
function gitTransform(line: string): GitOp | null { ... }

// Execute notifications
function notifyExecutor(op: GitOp) { ... }

// Compose the pipeline
pipe(gitLogWatcher(), gitTransform, notifyExecutor);
```

## Anti-Patterns (What We Avoided)

### ❌ Service Classes
```typescript
// Don't do this
class FileWatcherService extends EventEmitter {
  constructor(private config: Config) {}
  start() { ... }
  stop() { ... }
}
```

### ❌ Complex Event Systems
```typescript
// Don't do this
eventBus.on('file:changed', (e) => {
  eventBus.emit('transform:needed', e);
});
```

### ❌ Generic Abstractions
```typescript
// Don't do this
interface Pipeline<T extends Watchable, U extends Transformable> {
  // ... complex type gymnastics
}
```

## Current Implementation Stats
- **Total Lines**: ~150
- **Core Pattern**: 15 lines (pipe function)
- **Main Pipeline**: ~50 lines
- **Supporting Code**: ~85 lines

## Why This Works

1. **Clarity**: Each piece does one thing
2. **Testability**: Pure functions, clear boundaries  
3. **Extensibility**: Add stages without touching existing code
4. **Debuggability**: Linear flow, obvious data path
5. **Maintainability**: Less code = less bugs

## Future Patterns

### Conditional Execution
```typescript
pipe(
  watcher,
  transform,
  conditionalExecutor(predicate, executorA, executorB)
);
```

### Parallel Pipelines
```typescript
multipipe([
  [watcherA, transformA, executorA],
  [watcherB, transformB, executorB]
]);
```

### Pipeline Composition
```typescript
const filePipeline = createPipeline(fileWatcher, fileTransform);
const gitPipeline = createPipeline(gitWatcher, gitTransform);
mergePipelines(filePipeline, gitPipeline, commonExecutor);
```

The WTE pattern provides infinite flexibility through simple composition.

## Tool Migration Pattern

### From Name-Based to Property-Based
The system has migrated from simple name-based tool identification to a richer property-based system:

```bash
# Old approach (deprecated)
read_pane_by_name "editor" 100

# New approach
capture_pane_with_properties \
  --source "claude-desktop" \
  --role "editor" \
  --last 100 \
  --grep "error" \
  --grep-context 2
```

### Migration Principles
1. **Preserve Functionality**: New tools must support all old capabilities
2. **Add Power**: Expose underlying system features (tmux capture-pane options)
3. **Property-Aware**: Use multi-dimensional property system for pane discovery
4. **Graceful Fallback**: Handle missing panes elegantly

### Example: capture_pane_with_properties
This tool demonstrates the migration pattern:
- Replaces simple `read_pane_by_name` 
- Adds full tmux capture-pane power (ranges, grep, formatting)
- Uses property-based pane discovery
- Maintains backward compatibility via name parameter

```javascript
// Rich parameter set exposing tmux capabilities
{
  // Property filters
  source?: 'user' | 'claude-desktop' | 'claude-code' | 'system',
  role?: 'editor' | 'terminal' | 'logs' | 'tests' | 'debug' | 'monitor',
  name?: string,
  
  // Capture options
  start?: number | '-',
  end?: number | '-', 
  last?: number,
  
  // Output formatting
  join_lines?: boolean,
  escape_sequences?: boolean,
  preserve_trailing?: boolean,
  
  // Search capabilities
  grep?: string,
  grep_context?: number,
  invert_match?: boolean
}
```

This pattern will guide future tool migrations, ensuring we expose system power while maintaining elegant APIs.

## CLI-First Pattern

### Direct Script Invocation
Instead of CLI → MCP → Script, we use CLI → Script directly:

```bash
# Old pattern (avoided)
cafe pane assign editor → HTTP POST → MCP server → assign-properties.sh

# New pattern (preferred)  
cafe pane assign editor → assign-properties.sh
```

Benefits:
- Faster execution (~10ms vs ~100ms)
- Simpler error handling
- Easier debugging
- No server dependency for CLI

### CLI Command Structure
```bash
#!/bin/bash
# Main cafe entry point
case "$1" in
  init)    exec "$CAFEDELIC_DIR/cli/commands/init.sh" "$@" ;;
  make)    exec "$CAFEDELIC_DIR/cli/commands/make.sh" "$@" ;;
  session) exec "$CAFEDELIC_DIR/cli/commands/session.sh" "$@" ;;
esac
```

## Reactive Database Display Pattern

Moving beyond send-keys to database-backed reactive displays:

```bash
# Old: Push text to panes
tmux send-keys -t $pane "Status: $message"

# New: Write to DB, pane reads and displays
sqlite3 $DB "INSERT INTO system_events (source, message) VALUES ('$source', '$message')"
# Pane runs: cafe events --follow
```

Benefits:
- Persistent history
- Rich queries
- Multiple consumers
- Structured data
## Session Management Pattern

Claude Code sessions tracked with human-friendly names:

```json
// ~/.cafedelic/sessions/active/frontend-refactor.json
{
  "name": "frontend-refactor",
  "pid": 12345,
  "started": "2025-06-12T10:30:00Z",
  "working_dir": "/home/alex/code/myapp/frontend",
  "last_command": "refactor auth component",
  "status": "active"
}
```

Pattern enables:
- Multiple concurrent sessions
- Human-readable identification  
- Status aggregation
- Session history

### Session Lifecycle
```bash
# Create session
cafe session new frontend-refactor
→ Creates JSON file in active/
→ Starts Claude Code with tracked PID

# List sessions
cafe session list
→ Reads all active/*.json files
→ Shows human-friendly names and status

# Archive session
cafe session done frontend-refactor  
→ Moves to history/
→ Adds completion timestamp
```

## Modular Layout Pattern

Layouts defined as composable configurations:

```bash
# Layout presets in ~/.cafedelic/layouts/
default.layout  # 30/70/30 standard
minimal.layout  # 50/50 simple
full.layout     # 5-pane comprehensive

# Applied via: cafe make --layout full
```

Each layout file contains:
- Split commands sequence
- Property assignments
- Initial commands for panes