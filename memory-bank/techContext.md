# Technical Context

## Technology Stack

### Core
- **Node.js**: Runtime environment
- **Express.js**: Minimal web framework for server
- **TypeScript**: Type safety and better tooling
- **Chokidar**: File watching library

### Future Additions
- **SQLite3**: Local intelligence storage
- **Claude API**: For automated analysis (claude -p)

## Development Environment

### Prerequisites
- Node.js 18+ (for native fs.watch improvements)
- TypeScript 5.x
- Unix-like environment (Linux/macOS)

### File Paths
- DC Logs: `~/.claude-server-commander/claude_tool_call.log`
- Future: Configurable log paths

## Architecture Decisions

### Why Express?
- Minimal overhead
- Well-understood patterns
- Easy to add endpoints
- Built-in event system

### Why Chokidar?
- Cross-platform file watching
- Handles edge cases well
- Position tracking support
- Proven in production

### Why TypeScript?
- Type safety for log structures
- Better IDE support
- Easier refactoring
- Self-documenting code

## Implementation Guidelines

### Code Structure
```
src/
├── services/
│   ├── watcher.service.ts
│   ├── translator.service.ts
│   └── analysis.service.ts
├── tools/
│   └── get-active-context.ts
├── utils/
│   └── logger.ts
└── index.ts
```

### Service Patterns
- Each service extends EventEmitter
- Services communicate via events
- No direct service dependencies
- Graceful error handling

### Performance Considerations
- Stream large files, don't load entirely
- Debounce rapid log changes
- Cache translations when possible
- Lazy load analysis features

## Deployment

### Local Development
```bash
npm install
npm run dev  # Runs with nodemon
```

### MCP Integration
Configure in Claude Desktop settings:
```json
{
  "cafedelic": {
    "command": "node",
    "args": ["/path/to/cafedelic/dist/index.js"]
  }
}
```

## Technical Constraints

1. **Single Process**: Keep it simple, no workers yet
2. **File-Based**: No network dependencies initially
3. **Stateless**: Restart-friendly design
4. **Memory-First**: Database is future enhancement
5. **Unix-Focused**: Optimize for developer environment
