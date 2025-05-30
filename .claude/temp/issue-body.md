# Implement Configurable Pane Routing for Shared Visibility System

## Overview

Cafedelic V2 has successfully implemented the WTE (Watch-Transform-Execute) pattern, reducing our codebase from ~2000 to ~150 lines while maintaining all functionality. However, the current implementation uses fixed pane destinations. We need to make this fully configurable via MCP tools to achieve our vision of a shared visibility system for Claude Desktop and Claude Code.

## Current State

- ✅ WTE pattern implemented and working
- ✅ File operation watching from MCP logs
- ✅ Claude Code operation watching
- ✅ Emacs integration functional
- ❌ No dynamic pane configuration
- ❌ No MCP tools for routing control
- ❌ Missing simple sh script implementations

## Vision

Create a configurable shared visibility system where:
1. **Immediate**: Claude Desktop/Code actions are visible in user-configured tmux panes
2. **Near-term**: Different types of output (files, logs, activity) route to different panes
3. **Future**: Full workspace orchestration ("set up workspace for marcel in window 3")

## Implementation Plan

### Phase 1: Core MCP Tools (Priority)
Replicate essential tools from deli as simple sh scripts:

**Pane Management Tools:**
- [ ] `assign_name_to_pane` - Name a pane for easy reference
- [ ] `read_pane_by_name` - Read content from named panes
- [ ] `send_to_pane` - Send text to named panes
- [ ] `list_named_panes` - Show all named panes
- [ ] `get_pane_details` - Get info about a named pane

**Routing Configuration Tools:**
- [ ] `set_output_destination` - Configure where specific output types go
- [ ] `get_routing_config` - Show current routing configuration
- [ ] `clear_routing` - Reset routing configuration

### Phase 2: Shell Script Foundation
Create simple sh scripts for core operations:

```bash
# Example: scripts/assign-pane-name.sh
#!/bin/bash
SESSION=$1
WINDOW=$2
PANE=$3
NAME=$4

# Store name in simple key-value file
echo "${SESSION}:${WINDOW}.${PANE}=${NAME}" >> ~/.cafedelic/pane-names
```

### Phase 3: WTE Integration
Connect routing configuration to existing pipelines:

1. **Watchers** read routing config
2. **Transforms** determine target pane based on operation type
3. **Executors** send output to configured panes

### Phase 4: Enhanced Visibility
Add new pipeline types:
- Git operation visibility
- Terminal command tracking
- Browser action monitoring
- Activity summaries

## Technical Approach

1. **Storage**: Simple file-based configuration in `~/.cafedelic/`
2. **Scripts**: Bash scripts in `scripts/` directory
3. **Integration**: Minimal changes to existing WTE pipelines
4. **MCP Tools**: Express endpoints that call sh scripts

## Success Criteria

- [ ] Can assign names to tmux panes via MCP tools
- [ ] Can configure routing destinations for different output types
- [ ] File operations appear in user-configured panes
- [ ] Configuration persists across restarts
- [ ] Zero hard-coded pane assumptions

## Example Usage

```javascript
// Configure routing
await setOutputDestination({
  type: 'file-operations',
  pane: 'editor-output'
});

// Name a pane
await assignNameToPane({
  session: '0',
  window: 1,
  pane: 2,
  name: 'editor-output'
});

// Now all file operations show in that pane
```

## Related Context

- Previous v2 redesign: #11
- WTE pattern documentation: memory-bank/systemPatterns.md
- Deli reference implementation: /home/alex/code/deli

## Notes

This maintains our philosophy of simple, composable tools. Each piece does one thing well, and they combine to create powerful workflows. The sh script approach ensures reliability and easy debugging.
