# DC Log Watcher Implementation Plan

## Feature Overview

Transform Desktop Commander's JSON logs into human-readable activity streams, providing real-time visibility into Claude Desktop's actions.

## Architecture

### Components

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│   Watcher    │────▶│  Translator  │────▶│   Activity   │
│   Service    │     │   Service    │     │    Store     │
└──────────────┘     └──────────────┘     └──────────────┘
       ▲                                          │
       │                                          ▼
┌──────────────┐                          ┌──────────────┐
│   DC Log     │                          │  MCP Tools   │
│    File      │                          │  Interface   │
└──────────────┘                          └──────────────┘
```

### Data Flow

1. **Log Entry**: DC writes JSON to log file
2. **Detection**: Watcher detects file change
3. **Parsing**: Extract new entries since last position
4. **Translation**: Convert to human-readable format
5. **Storage**: Keep in memory activity buffer
6. **Delivery**: Serve via MCP tools or endpoints

## Implementation Steps

### Step 1: Create WatcherService

```typescript
// services/watcher.service.ts
import { EventEmitter } from 'events';
import * as chokidar from 'chokidar';
import * as fs from 'fs/promises';

export class WatcherService extends EventEmitter {
  private lastPosition = 0;
  private watcher?: chokidar.FSWatcher;

  async start(logPath: string) {
    // Initialize position from file size
    const stats = await fs.stat(logPath);
    this.lastPosition = stats.size;

    // Watch for changes
    this.watcher = chokidar.watch(logPath, {
      persistent: true,
      usePolling: true,
      interval: 100
    });

    this.watcher.on('change', () => this.processNewEntries(logPath));
  }

  private async processNewEntries(logPath: string) {
    // Read new content since last position
    // Parse JSON lines
    // Emit events for each entry
  }
}
```

### Step 2: Create TranslatorService

```typescript
// services/translator.service.ts
interface CommandTemplates {
  [key: string]: (args: any) => string;
}

export class TranslatorService {
  private templates: CommandTemplates = {
    'read_file': (args) => 
      `Claude is reading ${path.basename(args.path)}`,
    'write_file': (args) => 
      `Claude is updating ${path.basename(args.path)}`,
    'edit_file': (args) => 
      `Claude is editing ${path.basename(args.file_path)}`,
    'search_code': (args) => 
      `Claude searched for "${args.pattern}" in ${args.path}`,
    'execute_command': (args) => 
      `Claude executed: ${args.command}`,
    'list_directory': (args) => 
      `Claude is exploring ${args.path}`,
    'create_directory': (args) => 
      `Claude created directory ${args.path}`
  };

  translate(entry: DCLogEntry): string {
    const time = this.formatTime(entry.timestamp);
    const template = this.templates[entry.command];
    
    if (!template) {
      return `[${time}] Claude used ${entry.command}`;
    }

    return `[${time}] ${template(entry.args)}`;
  }

  private formatTime(timestamp: string): string {
    // Convert to local time in h:mm format
  }
}
```

### Step 3: Create Activity Store

```typescript
// services/activity.store.ts
export class ActivityStore {
  private activities: Activity[] = [];
  private maxSize = 1000;

  add(activity: Activity) {
    this.activities.push(activity);
    if (this.activities.length > this.maxSize) {
      this.activities.shift();
    }
  }

  getRecent(minutes: number): Activity[] {
    const cutoff = Date.now() - (minutes * 60 * 1000);
    return this.activities.filter(a => a.timestamp > cutoff);
  }

  getSummary(): ActivitySummary {
    // Group by command type
    // Identify patterns
    // Extract files under review
  }
}
```

### Step 4: Wire Together

```typescript
// index.ts additions
const watcher = new WatcherService();
const translator = new TranslatorService();
const activityStore = new ActivityStore();

watcher.on('log-entry', (entry) => {
  const activity = {
    raw: entry,
    translated: translator.translate(entry),
    timestamp: Date.now()
  };
  
  activityStore.add(activity);
  
  // Emit for real-time subscribers
  eventBus.emit('activity', activity);
});

// Start watching
const DC_LOG_PATH = path.join(
  process.env.HOME, 
  '.claude-server-commander',
  'claude_tool_call.log'
);

watcher.start(DC_LOG_PATH);
```

## Testing Strategy

### Unit Tests
- Translator templates for each command
- Time formatting edge cases
- Activity store limits and queries

### Integration Tests
- File watching with mock logs
- End-to-end translation flow
- MCP tool responses

### Manual Testing
1. Start cafedelic server
2. Use Claude Desktop with DC
3. Verify translations appear
4. Test get_active_context tool

## Success Metrics

- [ ] Real-time log detection (<100ms)
- [ ] Accurate translations for common commands
- [ ] Memory-efficient activity storage
- [ ] Useful context summaries
- [ ] Zero tmux dependencies

## Future Enhancements

1. **Pattern Detection**: Identify workflow sequences
2. **Smart Summarization**: Group related activities
3. **File Graph**: Track file relationships
4. **Performance Metrics**: Command timing analysis
5. **Persistent Storage**: SQLite for history

## Error Handling

- Graceful handling of malformed logs
- Recovery from file access errors  
- Fallback for unknown commands
- Position tracking resilience
