# Feature: Output Routing Management System for Dynamic Source-to-Destination Assignment

## Overview
Create a flexible output routing system that allows dynamic assignment of different sources (like Claude Desktop logs, manual commands, etc.) to specific output destinations (tmux panes, terminals, etc.) instead of hard-coding destinations in individual services.

## Problem Statement
Currently, emacs integration hard-codes output destinations (e.g., session 9:0.2). This creates several issues:
- **Inflexibility**: Cannot easily change output destinations without code modifications
- **No Multi-Source Support**: Cannot route different types of operations to different panes
- **Poor Daemon Management**: No graceful handling of emacs daemon lifecycle
- **Scalability Issues**: Adding new sources or destinations requires scattered code changes

## Expected Behavior
1. **Dynamic Assignment**: Configure source → destination mappings without code changes
2. **Source Filtering**: Route specific commands (e.g., `list_directory`) to designated panes
3. **Daemon Management**: Automatically check and start emacs daemon when needed
4. **Runtime Configuration**: Add/modify/remove routing rules during system operation
5. **Graceful Fallbacks**: Handle missing destinations or daemon failures elegantly

## Architecture Proposal

### Core Components
```typescript
// Output Router Service
interface RoutingRule {
  id: string;
  source: string;        // 'claude-desktop-logs', 'manual-commands'
  sourceFilter?: string; // 'list_directory', 'read_file', etc.
  destination: string;   // 'session:window.pane' format
  enabled: boolean;
}

// Emacs Daemon Manager
class EmacsDaemonManager {
  async checkStatus(): Promise<{ running: boolean, pid?: number }>
  async startDaemon(): Promise<{ success: boolean, message: string }>
  async ensureRunning(): Promise<void>
}
```

### Configuration Example
```typescript
const routingRules = [
  {
    id: 'claude-desktop-dired',
    source: 'claude-desktop-logs',
    sourceFilter: 'list_directory',
    destination: '9:0.2',
    enabled: true
  },
  {
    id: 'claude-desktop-files',
    source: 'claude-desktop-logs',
    sourceFilter: 'read_file', 
    destination: '9:0.1',
    enabled: true
  }
];
```

## Implementation Scaffold

### New Services
- `src/services/output-router.service.ts` - Core routing logic
- `src/services/emacs-daemon-manager.service.ts` - Daemon lifecycle management

### MCP Tools
- `assign_output` - Create/modify routing assignments
- `list_assignments` - View current routing rules
- `remove_assignment` - Delete routing rules

### Configuration Integration
- Add routing section to `cafedelic.config.ts`
- Database storage for persistent routing rules
- Runtime rule modification without restart

## Benefits
- **Flexibility**: Easy reconfiguration of output routing
- **Scalability**: Simple addition of new sources (Claude Code) and destinations
- **Reliability**: Proper emacs daemon management with error handling
- **Observability**: Central management and monitoring of all routing rules
- **User Experience**: Dynamic configuration without system restarts

## Technical Considerations
- **Backwards Compatibility**: Ensure existing functionality continues to work
- **Error Handling**: Graceful failures when destinations don't exist
- **Performance**: Efficient routing lookup for high-frequency operations
- **Configuration Management**: Persistent storage with runtime updates
- **Testing**: Mock destinations for testing without requiring specific tmux setups

## Implementation Notes
Keep the initial scaffold flexible as the optimal shape isn't fully determined. Focus on:
1. Clean separation between routing logic and destination handling
2. Pluggable architecture for adding new destination types
3. Configuration-driven behavior with sensible defaults
4. Comprehensive error handling and logging

## Future Extensions
- Support for other destination types (files, network endpoints)
- Advanced filtering with regex patterns
- Conditional routing based on system state
- Integration with other development tools beyond emacs