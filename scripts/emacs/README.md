# Emacs Integration Scripts

This directory contains the working Cafedelic-Emacs integration using a left sidebar file tree.

## Architecture

```
┌─────────────────┬──────────────────────────┐
│ cafedelic/      │                          │
│ ├── README.md   │  [Current file content]  │
│ ├── src/        │                          │
│ │   └── index.ts│                          │
│ └── package.json│                          │
└─────────────────┴──────────────────────────┘
  ^ File tree        ^ Main editor area
    (30 chars)
```

## Core Scripts

### cafedelic-frame.el
Elisp functions for frame management:
- Left sidebar with file tree (30 chars wide)
- Integration with generate-file-tree.sh
- Hard-coded project root: `/home/alex/code/cafedelic`
- Debug logging with `[Cafedelic]` prefix

### init-claude-frame.sh
Initializes the two-pane layout with left sidebar tree.

### open-claude-file.sh
Opens a file and updates the tree display. Files appear immediately in the main area.

### clear-cafedelic-state.sh
Clears all Cafedelic state for a fresh start.

### diagnose-emacs.sh
Diagnostic tool showing:
- Window configuration
- Buffer list
- Tree content
- Recent files
- Project root

### test-stable-tree.sh
Main test script demonstrating the working solution.

## Usage

1. **Initialize the layout:**
   ```bash
   ./init-claude-frame.sh
   ```

2. **Open files:**
   ```bash
   ./open-claude-file.sh <file-path>
   ```

3. **Test the system:**
   ```bash
   ./test-stable-tree.sh
   ```

4. **Debug issues:**
   ```bash
   ./diagnose-emacs.sh
   ```

5. **Clear state:**
   ```bash
   ./clear-cafedelic-state.sh
   ```

## Current Limitations

- Tree state doesn't persist between sessions
- Manual file opening only (not connected to DC logs yet)
- Tree occasionally needs manual refresh

## Next Phase

- SQLite database for state persistence
- Connect to DC log watcher for automatic file opening
- Improved state management
- Tree refresh optimization
