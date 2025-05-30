# Cafedelic Watchers

## Available Watchers

### Claude Code Log Watcher
Watches Claude Code session logs and emits file operation events.

**Usage:**
```bash
# Run the formatted debug watcher
./scripts/watch-claude-code.sh

# Or run directly
node dist/test/debug-claude-code.js
```

**Output Format:**
```
[10:30:45] READ  /path/to/file.ts
[10:30:46] EDIT  /path/to/file.ts
           └─ "old code..." → "new code..."
[10:30:47] WRITE /path/to/newfile.ts
```

### MCP Log Watcher
Watches Claude Desktop MCP server logs for file operations.

**Usage:**
```bash
# Run the debug watcher
node dist/test/debug.js
```

## Integration

Both watchers integrate with pipelines to automatically open files in Emacs as soon as they're accessed:

```typescript
// Direct pipeline - immediate frame activation
import { claudeCodeToEmacs } from './pipelines/claude-code-to-emacs.js';
await runPipeline(claudeCodeToEmacs);
```

Files open in Emacs immediately when Claude accesses them, providing a responsive, real-time experience.