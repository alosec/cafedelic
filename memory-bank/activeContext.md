# Active Context

## Current State (2025-05-25)

### Major Pivot
Cafedelic has pivoted from being a tmux-based IDE launcher to a pure intelligence framework. This clarifies our purpose and simplifies implementation significantly.

### What Changed
- **Removed**: launch_ide tool and all tmux management
- **Removed**: Terminal launching functionality  
- **Added**: Clear focus on intelligence gathering
- **Added**: DC log translation as primary feature

### Decisions Made
1. **No Layout Management**: Users handle their own tmux setup
2. **Intelligence Only**: We provide understanding, not control
3. **Start Simple**: DC log watching as foundation
4. **High-Level Tools**: MCP interface for queries, not commands

## Next Immediate Steps

### 1. Remove Deprecated Code
- Delete `src/tools/launch-ide.ts`
- Delete `scripts/launch-terminal.sh`
- Update `index.ts` to remove launch_ide registration

### 2. Implement DC Log Watcher
Create basic watcher service:
- Monitor `~/.claude-server-commander/claude_tool_call.log`
- Parse JSON log lines
- Emit events for new entries

### 3. Create Translation Engine
Simple template-based translator:
- Map command names to human descriptions
- Handle common Desktop Commander commands
- Output readable activity stream

### 4. Update get_active_context
Rename and stub for future:
- Change to `get_active_context` 
- Return current activity summary
- Plan for richer context later

## Feature #1 Specification

### DC Log Watch & Translate

**Input**: Raw DC logs
```json
{"timestamp":"2024-05-25T15:42:33.921Z","command":"read_file","args":{"path":"/home/alex/project/README.md"}}
```

**Output**: Human readable
```
[3:42pm] Claude is reading README.md
```

**Implementation Plan**:
1. Create `WatcherService` with Chokidar
2. Create `TranslatorService` with templates
3. Wire together in Express server
4. Expose activity stream endpoint

## Architecture Sketch

```typescript
// services/watcher.service.ts
export class WatcherService extends EventEmitter {
  watch(logPath: string) {
    // Chokidar setup
    // Emit 'log-entry' events
  }
}

// services/translator.service.ts  
export class TranslatorService {
  translate(logEntry: LogEntry): string {
    // Template-based translation
  }
}

// index.ts
watcher.on('log-entry', (entry) => {
  const translated = translator.translate(entry);
  activityStream.push(translated);
});
```

## Open Questions

1. **Activity History**: How much history to keep in memory?
2. **Real-time Delivery**: WebSocket, SSE, or polling?
3. **Translation Detail**: How much context in messages?
4. **Error Handling**: How to handle malformed logs?

## Success Criteria

By end of session:
- [x] Memory bank established with clear vision
- [x] Deprecated code removed
- [x] Basic DC log watcher working
- [x] Simple translations displaying
- [x] get_active_context returning real data

## What's Working Now

1. **DC Log Watcher**: Polls log file every 500ms for new entries
2. **Translation Engine**: Converts 10+ common DC commands to readable text
3. **Activity Store**: Maintains recent activity in memory
4. **MCP Integration**: get_active_context returns real activity summaries
5. **Standalone Monitor**: scripts/monitor-dc-logs.js for testing (fixed import paths)

## Recent Fixes

- Fixed monitor script import paths to use `dist/src/services/` instead of `dist/services/`
- Monitor now runs successfully and watches for DC log changes

## Next Steps

1. **Testing**: Run with Claude Desktop to verify translations ✅
2. **Enhancement**: Add more command templates as discovered
3. **Emacs Integration**: Automatically open files Claude accesses (PLANNED)
4. **Polish**: Improve time formatting and file path handling
5. **Documentation**: Create user guide for setup and usage

## Emacs Integration Planning (2025-05-25)

Successfully demonstrated DC log monitoring with real output:
```
[7:09pm] Claude is exploring ~/code/teemax/memory-bank
[7:10pm] Claude used read_multiple_files
[7:10pm] Claude is reading .clauderules
```

This reveals the opportunity for active environment coordination:
- Extract file paths from DC logs
- Automatically open them in Emacs
- Create synchronized view of Claude's work

See `emacs-integration-plan.md` for detailed design.

## Emacs Integration Complete (2025-05-26)

### Working Solution Achieved
- Left sidebar file tree (30 chars wide)
- Stable two-pane layout
- Integration with generate-file-tree.sh
- Hard-coded project root for simplicity
- Files display immediately when accessed

### Key Implementation Details
- `cafedelic-frame.el`: Core elisp with proper window management
- Shell script integration better than complex elisp
- Debug logging helps troubleshoot issues
- State cleanup prevents buffer accumulation

### Ready for Next Phase
Need to connect to DC logs and add persistence:
1. SQLite database for state
2. Wire DC watcher to auto-open files
3. Enhanced state management
4. Activity analytics

### Scripts Cleaned Up
Removed 10 experimental scripts, kept 6 working ones:
- init-claude-frame.sh
- open-claude-file.sh
- clear-cafedelic-state.sh
- diagnose-emacs.sh
- test-stable-tree.sh
- cafedelic-frame.el
