# System Patterns

## Architecture Overview

Cafedelic follows a layered architecture focused on intelligence extraction:

```
Input Layer (Logs) → Translation Layer → Analysis Layer → Intelligence Layer
```

## Core Components

### 1. Watcher Service
- **Pattern**: File watching with position tracking
- **Implementation**: Chokidar or native fs.watch
- **Responsibility**: Monitor log files for changes
- **Key Decision**: Stateless watching with position memory

### 2. Translation Engine
- **Pattern**: Parser → Transformer → Formatter pipeline
- **Implementation**: Simple JSON parsing with template-based output
- **Responsibility**: Convert raw logs to human-readable messages
- **Key Decision**: Template-based translation for maintainability

### 3. Express Server
- **Pattern**: Event-driven architecture
- **Implementation**: Minimal Express.js setup
- **Responsibility**: Coordinate services and expose MCP tools
- **Key Decision**: Keep server thin, logic in services

### 4. Intelligence Store (Future)
- **Pattern**: Time-series data with queryable interface
- **Implementation**: SQLite3 with simple schema
- **Responsibility**: Accumulate and query project intelligence
- **Key Decision**: Start with memory, add persistence later

## Design Patterns

### Translation Templates
```javascript
const templates = {
  'read_file': (args) => `Claude is reading ${path.basename(args.path)}`,
  'write_file': (args) => `Claude is updating ${path.basename(args.path)}`,
  'execute_command': (args) => `Claude executed: ${args.command}`
}
```

### Event Flow
```
Log Change → Watcher → Parse → Translate → Emit → Display
                            ↓
                        Analyze → Store
```

### Service Isolation
Each service has a single responsibility:
- WatcherService: File monitoring only
- TranslationService: Log to human text only
- AnalysisService: Pattern detection only
- StorageService: Data persistence only

## Extension Points

### Adding New Log Sources
1. Create new watcher for log type
2. Add parser for log format
3. Register templates for translation
4. Connect to event bus

### Adding Intelligence Features
1. Subscribe to translation events
2. Apply analysis logic
3. Store insights
4. Expose via MCP tools

## Key Decisions

1. **No Tmux Dependencies**: Pure file and process watching
2. **Event-Driven**: Loose coupling between components
3. **Template-Based**: Easy to add new translations
4. **Progressive Enhancement**: Each layer adds value
5. **Stateless Where Possible**: Simplifies recovery and testing
6. **Shell Script Wrappers**: Leverage existing scripts via simple MCP wrappers

## Architectural Patterns

### Shell Script Wrapper Pattern
When functionality already exists as robust shell scripts:
```typescript
// Thin wrapper pattern
export async function mcpTool(params: ToolParams): Promise<Result> {
    const scriptPath = '/path/to/script.sh';
    const { stdout } = await execAsync(`${scriptPath} ${args}`);
    return parseOutput(stdout);
}
```

Benefits:
- Reuse battle-tested scripts
- Keep MCP tools simple
- Easy to maintain and debug
- Scripts can be used standalone

Applied to:
- TMEX layout tools (create_tmex_layout, capture_layout_state, clear_tmux_panes)
- Future shell-based integrations

## Anti-Patterns to Avoid

1. **Tight Coupling**: Don't mix watching with translation
2. **Complex Parsing**: Keep log parsing simple and forgiving
3. **Premature Optimization**: Start with working, optimize later
4. **Framework Overuse**: Use minimal dependencies
5. **Low-Level Control**: Stay at intelligence layer
