# Cafedelic - AI Development Transparency Framework

**⚠️ PRE-ALPHA STATUS ⚠️**

This project is in early experimental stages. While individual components work, the system is not cohesively usable yet. This is technical exploration of AI development transparency concepts, not production software.

## What This Project Explores

The core question: **What if AI development activity was visible and orchestratable?**

When Claude Desktop works on your code, you see results but not the journey. When Claude Code executes commands, the process is opaque. Cafedelic explores making AI development transparent through:

- **Watch**: Monitor AI activity logs in real-time
- **Translate**: Convert technical operations into human insights  
- **Route**: Send information to configurable display destinations
- **Context**: Maintain awareness of what AI assistants are doing

## Current State (Honest Assessment)

### What Actually Works ✅

**MCP Tools for Pane Management**
- Property-based tmux pane system (`@source` + `@role` + `@name`)
- 8 working MCP tools for Claude Desktop/Code integration
- Dynamic pane assignment without hard-coded coordinates
- Example: `assign_pane_properties` to make pane 0:0.1 "Claude Desktop's editor"

**Watch-Transform-Execute (WTE) Architecture**
- Clean functional pipeline pattern in ~150 lines of code
- Working file operation detection from MCP logs
- Automatic Emacs integration (opens files Claude accesses)
- Proven extensible pattern for new data sources

**Shell Script Foundation**
- Battle-tested Emacs integration scripts
- Pane-specific Emacs servers with auto-revert
- TMEX layout management tools
- Routing configuration persistence

### What's Experimental/Broken 🚧

**System Integration**
- Components exist but aren't cohesively orchestrated
- Configuration requires manual MCP tool calls
- No unified startup or management process
- Limited error handling and recovery

**User Experience**
- Requires deep technical knowledge to configure
- No guided setup or onboarding
- Documentation scattered across memory bank files
- Hard to know what's working vs what's aspirational

**Feature Completeness**
- Only file operations implemented in WTE pipeline
- Limited to Claude Desktop MCP log watching
- No Claude Code direct integration yet
- No persistence of insights or patterns

## Technical Architecture

### Core Pattern: Watch-Transform-Execute

```typescript
// The entire system follows this pattern
interface WTE<W, T, E> {
  watch: () => AsyncIterator<W>      // Observe data sources
  transform: (data: W) => T | null   // Shape data for action  
  execute: (action: T) => Promise<void> // Perform side effects
}

// Example: File operation pipeline
pipe(
  mcpLogWatcher('/home/alex/.config/Claude/logs/mcp-*.log'),
  fileOperationTransform,
  emacsExecutor('/path/to/open-file.sh')
);
```

### Property-Based Pane System

Multi-dimensional pane identification:
- **@source**: `user` | `claude-desktop` | `claude-code` | `system`
- **@role**: `editor` | `terminal` | `logs` | `tests` | `debug` | `monitor`  
- **@name**: Custom naming (backward compatibility)

```javascript
// Assign properties to panes
assign_pane_properties(session, window, pane, {
  source: 'claude-desktop',
  role: 'editor',
  name: 'main-editor'
});

// Find panes by properties
find_pane_by_source_and_role('claude-desktop', 'editor');
```

### MCP Tools Available

8 working tools for tmux pane management:
- `assign_pane_properties` - Multi-dimensional pane assignment
- `list_panes_by_properties` - Property-based pane discovery
- `find_pane_by_source_and_role` - Exact property matching
- `capture_pane_with_properties` - Advanced pane content capture
- `send_keys_to_pane` - Send text to named panes
- `send_special_key_to_pane` - Send special keys (ctrl-c, enter, etc.)
- `send_ctrl_c_to_pane_by_name` - Interrupt processes
- `get_details_for_pane_by_name` - Pane status information

## Project Structure

```
cafedelic/
├── README.md                 # This file
├── memory-bank/             # Context management documentation
│   ├── projectbrief.md      # Core vision and goals
│   ├── activeContext.md     # Current work and status
│   └── ...                  # Architecture and progress docs
├── src/                     # V2 WTE implementation
│   ├── mcp-tools/          # MCP server for Claude integration
│   ├── watchers/           # Log file monitoring
│   ├── transforms/         # Data transformation
│   └── executors/          # Action execution
├── scripts/                # Shell script foundation
│   ├── emacs/              # Emacs integration scripts
│   ├── pane-management/    # Tmux pane tools
│   └── routing/            # Output routing system
└── tmux-pane-management-guide.md  # Comprehensive MCP tools guide
```

## What's Interesting Here

Even in pre-alpha state, this project demonstrates several valuable concepts:

**AI Development Transparency**
- Real-time visibility into Claude Desktop file operations
- Translation of technical logs into human-readable activity
- Context preservation across AI interactions

**Flexible Output Routing**
- No hard-coded tmux destinations
- Property-based pane assignment
- Runtime reconfiguration without code changes

**Clean Architecture Patterns**
- WTE pattern scales from simple to complex pipelines
- Functional composition over object hierarchies
- Shell scripts for proven operations, TypeScript for orchestration

**MCP Integration Patterns**
- Property-based pane management
- Multi-dimensional tool organization
- Graceful fallbacks and error handling

## Current Limitations

**Not Production Ready**
- Requires manual configuration of each component
- Error handling is basic
- No unified management interface
- Limited testing and validation

**Limited Scope**
- Only monitors Claude Desktop MCP logs
- File operations only (no git, terminal, etc.)
- Single pipeline model
- No intelligence accumulation or pattern analysis

**Technical Debt**
- Configuration scattered across files
- Some tools deprecated during development
- Documentation in flux
- Integration points not fully connected

## Running the Components

**Start MCP Server (for Claude Desktop)**
```bash
node mcp-stdio.js
```

**Start WTE File Pipeline**
```bash
npm run build
npm start
```

**Use MCP Tools (via Claude Desktop)**
```javascript
// Example: Set up a development environment
assign_pane_properties(session: 'dev', window: 0, pane: 0, {
  source: 'claude-desktop',
  role: 'editor'
});
```

## Development Philosophy

This project follows "austere minimalism" - preferring inconvenience from refusing complexity over complexity itself. The goal is understanding and transparency, not control or automation.

**What Cafedelic IS**:
- A development intelligence framework
- A context analysis engine
- A translator of opaque logs into human understanding

**What Cafedelic IS NOT**:
- A tmux manager or layout tool
- A replacement for existing development tools
- A production-ready system (yet)

## Future Vision

The long-term vision involves:
1. **Phase 1** (current): Basic AI activity transparency
2. **Phase 2**: Multi-assistant coordination 
3. **Phase 3**: Pattern recognition and workflow optimization
4. **Phase 4**: Accumulated development intelligence

But that's the vision. Right now, we have interesting pieces exploring these concepts.

## For Technical Explorers

If you're interested in AI development transparency concepts, the most stable components to examine are:

1. **MCP Tools**: `src/mcp-tools/` - Working pane management for Claude
2. **WTE Pattern**: `src/` - Clean pipeline architecture
3. **Shell Scripts**: `scripts/` - Proven integration utilities
4. **Documentation**: `memory-bank/` - Detailed technical context

Expect to read code and documentation to understand what's working vs what's aspirational.

---

**Status**: Pre-alpha technical exploration  
**Audience**: Technical developers interested in AI development transparency  
**Expectation**: Interesting concepts, not production software
