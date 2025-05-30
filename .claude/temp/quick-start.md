# Quick Start Examples

## For Developers

### Basic Setup
```bash
# 1. Start cafedelic
npm start

# 2. In Claude, name your panes
assignNameToPane({ session: '0', window: 0, pane: 0, name: 'editor' })
assignNameToPane({ session: '0', window: 0, pane: 1, name: 'logs' })

# 3. Configure routing
setOutputDestination({ type: 'files', pane: 'editor' })
setOutputDestination({ type: 'activity', pane: 'logs' })
```

### Claude Code Integration
```bash
# Name the Claude Code pane
assignNameToPane({ session: '0', window: 1, pane: 0, name: 'claude-code' })

# Exit Claude Code when needed
sendCtrlCToPane({ name: 'claude-code', double_tap: true })
```

### Advanced Routing
```bash
# Assign semantic roles
assignPaneRole({ pane: 'editor', role: 'editor-output' })
assignPaneRole({ pane: 'logs', role: 'activity-monitor' })

# Route by role
setOutputDestination({ type: 'files', role: 'editor-output' })
```

## For End Users

Once configured, the system works transparently:
- File operations automatically appear in designated panes
- Activity summaries show in log panes
- No manual intervention needed
- Persists across sessions
