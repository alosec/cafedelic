# Active Context

## Current State
- **Date**: May 31, 2025
- **Focus**: Property-based system consolidation and bug fixes
- **Status**: `capture_pane_with_properties` tool fixed and fully functional

## Recent Changes
- **FIXED**: `capture_pane_with_properties` tool bug (2025-05-31)
  - Fixed parameter validation issue when no properties specified
  - Added intelligent fallback to current pane when no selection criteria provided
  - Corrected argument order for `find-best-pane-for-role.sh` (ROLE first, then SOURCE)
  - Improved error messages with specific criteria feedback
  - Added source-only selection logic for flexible pane discovery
  - Tool now works for all usage scenarios: property-based, name-based, and default
- **COMPLETED**: Pane assignment for Claude Desktop
  - Set pane 1:0.1 as claude-desktop's editor using `assign_pane_properties`
  - Verified property storage using tmux @source and @role options
  - Tested property-based pane discovery works correctly
- **COMPLETED**: Tool migration - Replace read_pane_by_name with capture_pane_with_properties
  - Removed obsolete `read_pane_by_name` tool from MCP server
  - Added powerful `capture_pane_with_properties` tool with full tmux capture-pane features
  - Supports property-based filtering (source, role, name)
  - Includes advanced capture options (range, grep, formatting)
  - Created supporting shell script for property-aware pane capture
- **COMPLETED**: Issue #16 - Consolidated to property-based approach
  - Removed hard-coded pane destinations from emacs.ts
  - Integrated property-based pane discovery using @source and @role
  - Deleted duplicate property-based-emacs.ts file
  - Updated pipelines to specify appropriate sources (claude-desktop, claude-code)
  - Simplified codebase by removing dual routing approaches
- **IMPLEMENTED**: Issue #16 Multi-dimensional pane property system
  - Added orthogonal `@source` and `@role` properties to panes
  - Created property-based discovery scripts with fallback logic
  - Enhanced MCP tools while maintaining backward compatibility
  - Built property-aware executors for WTE pipeline
- **RESOLVED**: Issue #14 Docker build failure from V1/V2 mixed state
- Successfully cherry-picked modern MCP server implementation from backup branch
- Updated to McpServer API with modern .tool() syntax and Zod validation
- Verified Docker build works with modern MCP implementation
- All 10 MCP tools functional with new API

## Property System Architecture
The new system adds two orthogonal dimensions to pane management:
- **@source**: "user" | "claude-desktop" | "claude-code" | "system"
- **@role**: "editor" | "terminal" | "logs" | "tests" | "debug" | "monitor"
- **@name**: Existing naming system maintained for backward compatibility

### Benefits
- Multiple AI assistants can have separate panes for same role
- Survives tmux layout changes (no hardcoded coordinates)
- Graceful fallbacks when specific panes don't exist
- Natural language setup: "Make this pane my Claude Desktop editor"

## Current Architecture (V2 - WTE)
```typescript
// Core pattern - entire system in ~150 lines
pipe(
  mcpLogWatcher(logPath),
  fileOperationTransform,
  emacsExecutor(scriptPath)
);

// Property-based executor with smart pane discovery
await openInEmacs(action, {
  source: 'claude-desktop',  // or 'claude-code', 'user'
  role: 'editor'            // or 'terminal', 'logs', etc.
});
```

### Key Components
1. **Watch**: Direct MCP log observation via readline
2. **Transform**: Simple JSON parsing and filtering
3. **Execute**: Shell script execution for Emacs
4. **Compose**: Functional pipeline with `pipe()` utility

## Implementation Status
- ✅ Core WTE pattern implemented
- ✅ MCP log watching functional
- ✅ Emacs integration working
- ✅ File operations pipeline complete
- ✅ 10x code reduction achieved
- ✅ Merged to main and deployed

## Ready For Next Phase

### Claude Desktop Integration
- Framework ready for shared workspace visibility
- Simple pipeline makes feature addition straightforward
- Can add new watchers/transforms with minimal effort
- Clean composition model supports any data flow

### Claude Code Integration
- Architecture supports direct tool integration
- No complex abstractions to navigate
- Each pipeline stage is independent and testable
- Easy to add new execution targets

## Next Steps
1. Implement Claude Desktop visibility features
   - Add more transform types for different operations
   - Create visual feedback pipelines
   - Build activity monitoring transforms

2. Add Claude Code tool integration
   - Create watchers for Code-specific operations
   - Build transforms for code analysis
   - Add executors for code actions

3. Enhance pipeline capabilities
   - Add filtering and routing options
   - Build conditional execution paths
   - Create pipeline composition tools

## Technical Achievement
- **Before**: 17 files, complex services, generic hell
- **After**: Single entry point, clear data flow, functional composition
- **Result**: Same functionality, 10x less code, infinitely more maintainable

## Available MCP Tools (Enhanced with Properties)
The MCP server now provides 8 property-aware tools:

### Property-Based Tools
- `assign_pane_properties` - Set name, source, and/or role on a pane
- `list_panes_by_properties` - Filter panes by any property combination
- `find_pane_by_source_and_role` - Direct lookup with exact match
- `capture_pane_with_properties` - Advanced pane capture with property filtering
  - Supports full tmux capture-pane options (range, grep, formatting)
  - Property-based pane discovery (source, role, name)
  - Built-in search/filter capabilities

### Pane Interaction Tools
- `send_keys_to_pane` - Send text/keys to named pane
- `send_special_key_to_pane` - Send special keys (enter, ctrl-c, etc.)
- `send_ctrl_c_to_pane_by_name` - Send Ctrl-C with double-tap option
- `get_details_for_pane_by_name` - Get pane information

### Deprecated Tools
- `read_pane_by_name` - Removed in favor of `capture_pane_with_properties`
- `assign_name_to_pane` - Removed in favor of `assign_pane_properties`

## Development Velocity
With the v2 architecture, adding new features is now:
1. Add a watcher (if new data source)
2. Add a transform (for new data shape)
3. Add an executor (for new action)
4. Compose with `pipe()`

No more navigating service hierarchies or fighting TypeScript generics.