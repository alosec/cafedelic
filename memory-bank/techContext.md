# Technical Context

## Technology Stack

### Core Runtime
- **Node.js 18+**: Modern JavaScript runtime
- **TypeScript 5.x**: Type safety and better tooling
- **Express.js**: Minimal web framework for MCP server
- **@modelcontextprotocol/sdk**: MCP server implementation

### Service Dependencies
- **Chokidar**: Cross-platform file watching
- **SQLite3**: Lightweight database (future persistence)
- **Winston**: Structured logging
- **EventEmitter**: Built-in event system

### Shell Integration
- **Bash Scripts**: Proven tmux/emacs operations
- **TMEX**: Tmux layout automation tool
- **Tmux**: Terminal multiplexer (user-provided)
- **Emacs**: Editor with server mode support

## File Structure

```
cafedelic/
├── src/
│   ├── services/          # Core service implementations
│   │   ├── desktop-mcp-watcher.service.ts
│   │   ├── translator.service.ts
│   │   ├── routing-manager.service.ts
│   │   └── activity-store.service.ts
│   ├── tools/             # MCP tool implementations
│   │   ├── set-editor-destination.tool.ts
│   │   ├── get-active-context.ts
│   │   └── create_tmex_layout.ts
│   ├── utils/             # Shared utilities
│   └── types/             # TypeScript type definitions
├── scripts/               # Shell script collection
│   ├── emacs/
│   │   └── pane-server/   # Pane-specific emacs
│   └── tmux/              # Layout management
├── memory-bank/           # Project documentation
└── dist/                  # Compiled output
```

## Key File Paths

### Logs
- Claude Desktop MCP: `/home/alex/.config/Claude/logs/mcp-server-*.log`
- Desktop Commander: `mcp-server-desktop-commander.log`
- Cafedelic Server: `mcp-server-cafedelic.log`

### Scripts
- Emacs Integration: `/home/alex/code/cafedelic/scripts/emacs/pane-server/`
- TMEX Layouts: `/home/alex/code/cafedelic/scripts/tmux/`

## Build & Development

### Setup
```bash
npm install
npm run build
```

### Development Mode
```bash
npm run dev  # Watches for changes
```

### MCP Registration
Add to Claude Desktop settings:
```json
{
  "cafedelic": {
    "command": "node",
    "args": ["/home/alex/code/cafedelic/dist/index.js"]
  }
}
```

## Architecture Decisions

### Why Event-Driven?
- Loose coupling between services
- Easy to add new components
- Natural for log streaming
- Supports future distribution

### Why Shell Scripts?
- Tmux operations are complex
- Scripts are battle-tested
- Easy to debug standalone
- Community knowledge

### Why No Persistence Yet?
- Focus on core functionality first
- Memory is sufficient for session
- Reduces complexity
- Easy to add later

## Performance Considerations

- **Log Polling**: 500ms intervals (configurable)
- **Discovery**: 30-second intervals for new logs
- **Streaming**: Process logs incrementally
- **Async Everything**: Non-blocking operations

## Error Handling

- **Graceful Degradation**: Features fail independently
- **User Feedback**: Clear error messages in responses
- **Logging Levels**: Configurable verbosity
- **Recovery**: Automatic retry for transient failures

## Future Technical Additions

### Planned
- SQLite persistence for routing config
- WebSocket for real-time UI updates
- Pattern detection algorithms
- Multi-agent message passing

### Considered
- Redis for distributed state
- GraphQL for complex queries
- Docker containerization
- Kubernetes orchestration
