# Cafedelic

Development intelligence framework that makes AI-assisted development transparent and analyzable.

## What is Cafedelic?

Cafedelic watches AI development tools like Claude Desktop and translates their activities into human-readable insights. Instead of seeing opaque logs and mysterious file changes, developers get clear visibility into what their AI assistants are doing.

## Philosophy

Cafedelic follows an austere, minimal approach. We prefer simplicity over complexity, focusing on providing intelligence and understanding rather than control.

## Current Features

- **Real-time Activity Monitoring**: Watches Desktop Commander logs and translates them
- **Human-Readable Translations**: Converts technical commands into plain English
- **Activity Summaries**: Provides context about recent development activities
- **Pattern Detection**: Identifies workflow patterns (heavy editing, exploration, testing)

## Example Output

```
[3:42pm] Claude is reading authentication module
[3:42pm] Claude searched for "JWT" in src/
[3:43pm] Claude is updating auth.service.ts
[3:43pm] Claude executed: npm test auth
```

## Installation

```bash
npm install
npm run build
```

## Usage

### As MCP Tool in Claude Desktop

Add to your Claude Desktop configuration:

```json
{
  "cafedelic": {
    "command": "node",
    "args": ["/path/to/cafedelic/dist/index.js"]
  }
}
```

Then use the tool:
```javascript
await get_active_context({ lookback_minutes: 10 })
```

### Standalone Monitor

Watch DC logs in real-time:
```bash
node scripts/monitor-dc-logs.js
```

## Architecture

Cafedelic is built as a pure intelligence layer:

- **No tmux management** - We don't control terminals or layouts
- **No low-level operations** - We observe and understand, not manipulate
- **Event-driven design** - Loosely coupled services that do one thing well
- **Template-based translation** - Easy to extend with new command types

## Development

```bash
npm run dev   # Build and run with auto-rebuild
npm test      # Run tests (coming soon)
```

## Roadmap

### Phase 1: Watch & Translate ✅
- Monitor Desktop Commander logs
- Translate to human-readable activity
- Provide basic summaries

### Phase 2: Analyze & Understand (Next)
- Detect workflow patterns
- Track file relationships
- Measure productivity metrics

### Phase 3: Accumulate Intelligence (Future)
- Build project knowledge over time
- Provide historical insights
- Enable cross-session learning

## Contributing

Cafedelic values simplicity and clarity. When contributing:
- Keep services single-purpose
- Prefer simple solutions over clever ones
- Document your discoveries in .clauderules
- Test with real usage before adding complexity

## License

MIT
