# System Patterns

## Architecture Overview

Cafedelic follows an event-driven architecture with flexible routing:

```
Input Layer → Translation Layer → Routing Layer → Display Layer
   (Logs)        (Human Text)      (Dynamic)       (Tmux/Emacs)
```

## Core Patterns

### 1. Shell Script Wrapper Pattern
**Problem**: Complex tmux/emacs operations need reliable execution
**Solution**: MCP tools as thin wrappers around proven shell scripts

```typescript
export async function createTmexLayout(params) {
  const scriptPath = path.join(SCRIPTS_DIR, 'tmux/create-tmex-layout.sh');
  const result = await executeScript(scriptPath, [params.targetPane, params.layout]);
  return parseScriptOutput(result);
}
```

**Benefits**:
- Reuse battle-tested scripts
- Easy debugging (scripts work standalone)
- Clear separation of concerns
- Bypass MCP parameter complexities

### 2. Dynamic Routing Pattern
**Problem**: Hard-coded destinations break with different layouts
**Solution**: User-configurable routing with runtime assignment

```typescript
class RoutingManager {
  async setDestination(role: string, paneSpec: string) {
    // Validate pane exists
    // Configure services
    // Start pane-specific servers
    // Emit configuration events
  }
}
```

### 3. Event-Driven Service Communication
**Problem**: Tight coupling between services
**Solution**: Services extend EventEmitter, communicate via events

```typescript
// Loose coupling via events
watcher.on('log-entry', (entry) => translator.process(entry));
translator.on('activity', (activity) => router.route(activity));
router.on('route', (dest, content) => display.update(dest, content));
```

### 4. Pane-Specific Server Pattern
**Problem**: Single emacs daemon conflicts with multiple panes
**Solution**: Independent emacs servers per pane

```bash
# Each pane gets its own server
start-pane-emacs.sh "0:0.1"  # Creates server for specific pane
open-file-in-pane.sh "/path/file.ts" "0:0.1"  # Uses pane's server
```

### 5. Progressive Enhancement Pattern
**Problem**: Complex features before basics work
**Solution**: Layer functionality progressively

```
1. Basic log watching → Works
2. Add translation → Enhanced
3. Add routing → Flexible  
4. Add auto-open → Integrated
5. Add persistence → Future
```

## Service Patterns

### Single Responsibility Services
```typescript
WatcherService      // ONLY watches files
TranslatorService   // ONLY translates logs
RoutingManager      // ONLY manages routes
ActivityStore       // ONLY stores activities
```

### Graceful Degradation
```typescript
async function openInEmacs(file: string, pane: string) {
  try {
    await triggerEmacsOpen(file, pane);
  } catch (error) {
    logger.warn('Emacs open failed, continuing', { error });
    // Don't break the flow
  }
}
```

### Configuration Discovery
```typescript
// Discover actual environment
const logs = await discoverDesktopMCPLogs();
const panes = await discoverTmuxPanes();
// Adapt to what exists
```

## Anti-Patterns Avoided

1. **Hard-Coded Assumptions**: No fixed sessions, panes, or layouts
2. **Synchronous Blocking**: All I/O operations are async
3. **Direct Service Coupling**: Services don't import each other
4. **Complex State Management**: Keep state minimal and local
5. **Premature Optimization**: Simple solutions first

## Extension Patterns

### Adding New Log Sources
1. Create watcher for log type
2. Add parser for format
3. Emit standardized events
4. Existing pipeline handles rest

### Adding New Display Targets
1. Implement display provider
2. Register with routing manager
3. Handle routing events
4. Manage target lifecycle

## Key Architectural Decisions

1. **Events Over Imports**: Services communicate via events
2. **Scripts Over Complexity**: Shell scripts for system operations
3. **Discovery Over Configuration**: Find what exists, adapt
4. **User Control Over Automation**: Let users decide routing
5. **Reliability Over Features**: Working basics before advanced
