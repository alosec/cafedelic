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
6. **Tmex Integration**: Created test-pane.sh for rapid layout testing ✅

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

## Tmex Layout Testing Tools (2025-05-26)

### New Tool Created
- `scripts/tmex-tools/test-pane.sh`: Interactive tmex pattern tester
- Tests layouts in adjacent pane without disrupting workflow
- Simple REPL interface: type pattern → see result
- Commands: pattern, clear, help, quit
- Supports transpose flag for horizontal layouts

### Usage
```bash
# Split window and run tester
tmux split-window -h
./scripts/tmex-tools/test-pane.sh

# Try patterns
tmex> 113        # Three columns
tmex> 1[123]1    # Nested layout
tmex> 131 -t     # Transposed
tmex> clear      # Reset
```

Perfect for rapid tmex pattern discovery and cafedelic layout development.

## Tmux Pane Management Resolution (2025-05-26)

### 🚨 Title Setting Complexity Removed
After extensive experimentation with pane titles (20+ test scripts), discovered that complex title management was causing more problems than it solved. Decision made to:
- Remove all title-setting infrastructure
- Use tmux pane names (@pane_name) instead
- Focus on core cafedelic functionality

### ✅ Clean Working State Achieved
- Removed 18 title-related test scripts
- Simplified create-ide-layout.sh
- Preserved tmex and tmex-tools for layout management
- Committed all valuable changes

### 💡 Key Lesson
**Avoid infrastructure rabbit holes** - Complex title setting was distracting from cafedelic's core mission of development intelligence.

## Current IDE Layout Solution

### Working Implementation
- Uses `tmex 131 --transpose` for consistent 5-pane layout
- Sets pane names using tmux user options (@pane_name)
- No dependencies on complex title infrastructure
- Clean, maintainable script

### Pane Structure
```
┌─────────────────────────────┐
│      Activity Monitor       │ Pane 0: @pane_name=cafedelic-activity
├──────┬──────────┬──────────┤
│ Tree │  Editor  │ Terminal │ Panes 1-3: files, editor, terminal
├──────┴──────────┴──────────┤
│        DC Logs              │ Pane 4: @pane_name=cafedelic-logs
└─────────────────────────────┘
```

Reference panes with: `tmux display -p -t '@pane_name=cafedelic-editor' '#{pane_id}'`

### Previous Context

### create-ide-layout.sh Updated
- Now uses `tmex 131 --transpose` for consistent 5-pane layout
- Sets pane titles with `-T` flag (standard tmux approach)
- Fixed monitor-dc-logs.js path (scripts/ not dist/)
- Launches appropriate content in each pane:
  - **activity**: Terminal (top) - future features planned
  - **filetree**: File tree viewer (left)
  - **editor**: Emacs (center)
  - **terminal**: Shell (right)
  - **logs**: DC log monitor (bottom)

### Tmux Config Updated
- Changed pane-border-format to use `#{pane_title}` instead of custom variable
- Ensures pane titles set by scripts are displayed correctly

### Usage
```bash
./scripts/create-ide-layout.sh [session-name] [window-name] [project-dir]
```

Predictable layout every time, named panes for easy targeting.


## State Management Implementation (2025-05-26)

### Major Progress: Context Persistence Foundation
Successfully implemented the foundational components of the context persistence model:

### ✅ Completed Today
1. **Database Schema**: Created comprehensive SQLite3 schema with tables for:
   - File access tracking
   - Activity logging
   - Pane state management
   - Session tracking
   - File relationships

2. **Core Services**: 
   - `PersistenceService`: SQLite3 database operations
   - `StateManager`: Central event-driven state coordination
   - Integrated with existing DC log watcher

3. **Tmux Tools Started**:
   - `split_pane_horizontal`: Split panes horizontally with naming
   - `split_pane_vertical`: Split panes vertically with naming
   - Registered as MCP tools

4. **Emacs Simplification**:
   - Moved file tree from emacs to tmux pane 1 (as requested)
   - Created `cafedelic-editor.el` - simple editor without tree
   - Updated scripts to use new configuration

### Current Architecture
```
DC Logs → WatcherService → StateManager → Database
                ↓              ↓
         TranslatorService   Events
                ↓              ↓
          ActivityStore    UI Updates
```

### Database Status
- Schema created with proper indexes
- Event-sourced design for full history
- Views for common queries (recent_activities, active_context_files)

## Next Immediate Steps

1. **Test Database Integration**
   - Run test-db.sh to verify SQLite initialization
   - Verify file access tracking works
   - Test with real DC logs

2. **Complete Pane Coordinator**
   - Create `pane-coordinator.service.ts`
   - Manage pane states
   - Coordinate file display
   - Handle pane events

3. **Wire DC Logs to File Opening**
   - When DC accesses a file, auto-open in editor
   - Track which files are in which panes
   - Update pane states in database

4. **Enhanced Tmux Tools**
   - `get_full_window_by_name`
   - `get_full_pane_by_name`
   - Grid layout management

## Integration Points Working

1. **State Manager Connected**: Receives events from DC watcher
2. **Database Ready**: SQLite3 schema deployed
3. **Emacs Simplified**: Editor-only mode, file tree in tmux
4. **Tools Registered**: Split pane tools available

## Architecture Decisions

1. **Event-Sourced State**: All changes logged to database
2. **Loose Coupling**: Services communicate via events
3. **Progressive Enhancement**: Each feature works independently
4. **SQLite for Persistence**: Simple, reliable, no external dependencies

## Debug & Logging Information

### Key Log Locations
- **Desktop Commander Logs**: `~/.claude-server-commander/claude_tool_call.log` 
- **Claude Desktop MCP Logs**: `~/.config/Claude/logs/mcp-server-cafedelic.log`
- **Cafedelic Server Logs**: `{project-root}/server.log`
- **Cafedelic Database**: `{project-root}/cafedelic.db`

### MCP Registration Issues (2025-05-26)
**Root Cause Identified**: Claude Desktop runs MCP servers from its own working directory, not the project directory. When server uses `process.cwd()`, it gets Claude Desktop's path instead of cafedelic project path, causing schema.sql and database paths to fail.

**Solution**: Use module-relative paths (`__dirname`) instead of `process.cwd()` for all file paths in PersistenceService.

**✅ STATUS: RESOLVED** - Fixed by Marcel, MCP server now connecting successfully!

### MCP Protocol Compliance Pattern (2025-05-26)
**Critical Rule**: MCP servers must only write valid JSON to stdout. ALL debug/logging messages must use `console.error()` (stderr) instead of `console.log()` (stdout).

**Problem**: Emoji characters in console.log break JSON protocol:
```
Unexpected token '✅', "✅ Database"... is not valid JSON
```

**Solution Pattern**: 
- ✅ `console.error('✅ Debug message')` - Goes to stderr, safe for emojis
- ❌ `console.log('✅ Debug message')` - Goes to stdout, breaks MCP JSON

## 🎉 MAJOR SUCCESS (2025-05-26)

### Auto-Open Integration WORKING!
- **Live File Updates**: Files automatically open in Emacs as Claude accesses them
- **Real-time Coordination**: DC logs → StateManager → EmacsService → Sophie (emacs pane)
- **Seamless Experience**: Files appear instantly in editor as they're accessed
- **Powerful Foundation**: Simple server enabling extremely powerful workflows
