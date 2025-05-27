# Emacs Integration Improvements: Plain Mode and Overlay Fixes

## Problem Statement

Current emacs integration has several issues:
1. Relies entirely on emacs daemon/client architecture
2. Creates tmux overlay text that obscures the editor pane
3. No fallback for when daemon fails or isn't desired
4. Complex socket management adds failure points

## Proposed Solution

Implement a dual-mode emacs integration system:
- **Plain Mode**: Direct `emacs` command execution in target pane
- **Daemon Mode**: Current emacsclient approach (preserved)
- Configuration toggle to switch between modes
- Fix overlay display issues in both modes

## Implementation Plan

### 1. Plain Emacs Scripts

Create new shell scripts that use `tmux send-keys` to run emacs directly:

```bash
# scripts/emacs/open-file-plain.sh
#!/bin/bash
PANE="${CAFEDELIC_TARGET_PANE:-9:0.2}"
FILE="$1"

# Check if emacs is already running in pane
if tmux capture-pane -t "$PANE" -p | grep -q "GNU Emacs"; then
    # Send file open command to existing emacs
    tmux send-keys -t "$PANE" Escape ":e $FILE" Enter
else
    # Launch emacs with file
    tmux send-keys -t "$PANE" "emacs $FILE" Enter
fi
```

### 2. Configuration Enhancement

Add to `cafedelic.config.ts`:
```typescript
emacs: {
  useDaemon: boolean;        // Toggle daemon vs plain mode
  autoOpen: boolean;         // Existing
  showNotifications: boolean; // Control overlay messages
  targetPane: string;        // Which pane to use
}
```

### 3. Service Modifications

Update `EmacsService` to:
- Check `useDaemon` flag before daemon operations
- Route to appropriate scripts based on mode
- Optionally disable overlay notifications
- Handle both modes gracefully

### 4. Overlay Fix Options

Multiple approaches to fix the overlay issue:

**Option A: Remove notifications**
- Simply don't call `outputRouter.routeToPane()` for file opens

**Option B: Redirect to activity monitor**
- Route messages to Pierre (activity monitor) instead

**Option C: Use tmux display-message**
- Less intrusive notification method
```bash
tmux display-message -t "$PANE" "Opened: $FILE"
```

**Option D: Status line updates**
- Update tmux status line instead of pane content

## Testing Strategy

1. Create test script to verify both modes work
2. Test file opening in both plain and daemon mode
3. Test directory (dired) opening in both modes
4. Verify overlay issue is resolved
5. Test mode switching during runtime

## Success Criteria

- [ ] Plain emacs mode opens files reliably
- [ ] Daemon mode continues to work as before
- [ ] Easy configuration switching between modes
- [ ] No more overlay text obscuring editor
- [ ] Clear feedback when files are opened
- [ ] Graceful fallback when one mode fails

## Future Enhancements

- Auto-detect best mode based on environment
- Performance comparison between modes
- Integration with Docker testing framework (Issue #8)
- Support for other editors (vim, vscode, etc.)
