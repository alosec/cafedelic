# Active Context

## Current State
- **Date**: January 2025
- **Focus**: Multi-dimensional pane property system implementation
- **Status**: V2 redesign complete, property system for Issue #16 implemented

## Recent Changes
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

// NEW: Property-based routing
pipe(
  mcpLogWatcher(),
  fileOperationTransform,
  propertyBasedEmacsExecutor({
    defaultSource: 'claude-desktop',
    defaultRole: 'editor'
  })
);
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
All the MCP tools developed during v1 remain functional, plus new property-based tools:

### Existing Tools (Still Working)
- `assign_name_to_pane` - Now uses property system internally
- `setEditorDestination(paneSpec)` - Route to tmux panes
- `create_tmex_layout(targetPane, layout)` - Create layouts
- `toggle_auto_open(enable?)` - Control auto-opening
- Plus all other routing and layout tools

### New Property-Based Tools
- `assign_pane_properties` - Set name, source, and/or role on a pane
- `list_panes_by_properties` - Filter panes by any property combination
- `find_pane_by_source_and_role` - Direct lookup with exact match

## Development Velocity
With the v2 architecture, adding new features is now:
1. Add a watcher (if new data source)
2. Add a transform (for new data shape)
3. Add an executor (for new action)
4. Compose with `pipe()`

No more navigating service hierarchies or fighting TypeScript generics.