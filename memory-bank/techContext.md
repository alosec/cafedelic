# Technical Context

## Technology Stack

### Core Runtime
- **Node.js**: v20+ (async iterators, ES modules)
- **TypeScript**: Minimal typing, focus on inference
- **Shell Scripts**: Proven Emacs integration scripts

### Dependencies (Minimal)
- `glob`: File pattern matching
- `readline`: Built-in Node.js module for log tailing
- No complex frameworks or libraries

## Project Structure (V2)

```
cafedelic/
├── index.ts           # Entry point (~150 lines total)
├── core/              # WTE pattern implementation
│   ├── wte.ts        # Core types and interfaces
│   ├── compose.ts    # Pipe utility function
│   └── runner.ts     # Pipeline execution
├── watchers/          # Data source observers
│   └── mcp-log.ts    # MCP log file watcher
├── transforms/        # Data transformation functions
│   └── file-operations.ts  # Extract file ops from logs
├── executors/         # Side effect performers
│   └── emacs.ts      # Execute emacs commands
├── pipelines/         # Pre-composed pipelines
│   └── file-to-emacs.ts   # Main pipeline
└── scripts/           # Shell scripts (unchanged from v1)
    └── emacs/        # All emacs integration scripts
```

## Architecture Decisions

### Why WTE Pattern
- **Simplicity**: Three-phase model covers all needs
- **Composability**: Functions compose naturally
- **Testability**: Each phase is independently testable
- **Flexibility**: Easy to add new sources/transforms/actions

### Why Functional
- **No State**: Pipelines are stateless flows
- **No Classes**: Functions are sufficient
- **No Events**: Direct data flow instead
- **No Abstractions**: Code does what it says

### Shell Script Integration
All complex Emacs operations remain in battle-tested shell scripts:
- `open-claude-file.sh` - Opens files in Emacs
- `open-dired.sh` - Opens directories
- `pane-server/` - Pane-specific Emacs servers
- Plus 10+ other integration scripts

## Build & Run

### Development
```bash
# Install minimal dependencies
npm install

# Run in development
npm run dev

# Build for production
npm run build
```

### Production
```bash
# Start the server
npm start
```

## File Monitoring

### MCP Logs Location
```
/home/alex/.config/Claude/logs/mcp-*.log
```

### Log Format
```json
{
  "method": "read_file",
  "params": {
    "path": "/absolute/path/to/file"
  }
}
```

## Integration Points

### Tmux Integration
- Routes output to specific panes
- Supports any session/window/pane combination
- Configured via MCP tools

### Emacs Integration
- Daemon mode or pane-specific servers
- Auto-opens files on AI access
- Maintains separate frames per pane

## Configuration

### Environment Variables
```bash
# Optional - defaults work fine
CAFEDELIC_LOG_PATH="/custom/path/to/logs"
CAFEDELIC_SCRIPTS_PATH="/custom/path/to/scripts"
```

### MCP Tools Configuration
Tools from v1 still work for configuration:
- `setEditorDestination("0:0.1")` - Set output pane
- `toggle_auto_open(true)` - Enable auto-opening
- `set_emacs_mode("pane-server")` - Configure Emacs

## Performance Characteristics

### Resource Usage
- **Memory**: ~50MB (Node.js baseline)
- **CPU**: Near zero (event-driven)
- **Startup**: <1 second
- **Latency**: <100ms file open response

### Scalability
- Handles thousands of events/second
- No memory leaks (stateless design)
- Linear performance with pipeline count

## Error Handling

### Current Approach
- Errors logged to console
- Pipelines continue on non-fatal errors
- Failed transforms return null (skip)
- Failed executions logged but don't stop pipeline

### Future Considerations
- Add error recovery strategies
- Implement retry logic for executions
- Add metrics/monitoring hooks

## Testing Strategy

### Unit Tests
- Test transforms with known inputs
- Test executors with mocked shell commands
- Test watchers with synthetic events

### Integration Tests
- Test full pipelines with real files
- Verify Emacs integration works
- Test MCP tool compatibility

## Security Considerations

### File Access
- Only reads log files from Claude config
- Executes pre-defined shell scripts
- No arbitrary command execution

### Process Isolation
- Runs as user process
- No elevated privileges required
- Sandboxed to specific directories

## Future Technical Directions

### Potential Enhancements
1. **Multi-Pipeline Support**: Run multiple pipelines concurrently
2. **Plugin System**: Dynamic pipeline loading
3. **WebSocket Support**: Real-time browser updates
4. **Metrics Collection**: Performance monitoring

### Maintaining Simplicity
Any additions must:
- Follow WTE pattern
- Remain functional
- Avoid state management
- Keep code readable

The technical foundation is intentionally minimal to maximize maintainability and extensibility.