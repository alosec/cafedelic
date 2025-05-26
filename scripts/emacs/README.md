# Emacs Integration Scripts

This directory contains shell scripts that interface with Emacs via emacsclient.

## Current Implementation: Left Sidebar Tree

### Layout
```
┌─────────────────┬──────────────────────────┐
│ cafedelic/      │                          │
│ ├── README.md   │  [Current file content]  │
│ ├── src/        │                          │
│ │   └── index.ts│                          │
│ └── package.json│                          │
└─────────────────┴──────────────────────────┘
```

### Key Scripts

**cafedelic-frame.el** - Fixed frame management with:
- Proper window cleanup
- Left sidebar for tree (30 chars wide)
- Integration with generate-file-tree.sh
- Stable buffer management

**test-stable-tree.sh** - Test the new stable layout

**diagnose-emacs.sh** - Check buffer/window state

### Usage
```bash
# Initialize and test
./init-claude-frame.sh
./test-stable-tree.sh

# Diagnose issues
./diagnose-emacs.sh
```

## New Full Frame Scripts (Current Implementation)

### cafedelic-frame.el
Core elisp functions with minimal file tree display.
- Builds tree structure from file paths
- Groups by directories
- No decorative elements

### init-claude-frame.sh  
Initializes the full frame layout with file tree at top.

### open-claude-file.sh
Opens a file and updates the tree. Files display immediately for rapid flipping effect.

### test-rapid-flip.sh
Demonstrates rapid file access with tree view updates.

**Usage:**
```bash
# Initialize and test
./init-claude-frame.sh
./test-rapid-flip.sh

# Or open individual files
./open-claude-file.sh <file-path>
```

## Previous Scripts (Split Window Approach)

### open-file-test.sh
Our "hello world" prototype that tests basic file opening functionality.

**Usage:**
```bash
./open-file-test.sh <file-path>
```

### init-claude-view.sh
Sets up the two-window layout for Cafedelic.
- Left window: User's workspace (preserved)
- Right window: Claude's workspace

**Usage:**
```bash
./init-claude-view.sh
```

### open-right.sh
Opens a file in Claude's buffer (right window) with claude- prefix.
- Maintains two-window layout
- Reuses existing buffers if already open
- Returns focus to user's window

**Usage:**
```bash
./open-right.sh <file-path>
```

### test-window-management.sh
Demonstrates the complete workflow with multiple files.

**Usage:**
```bash
./test-window-management.sh
```

**Example:**
```bash
# Test with our test file
./open-file-test.sh test-file.md

# Test with absolute path
./open-file-test.sh /home/alex/code/cafedelic/README.md
```

**What it does:**
1. Checks if emacs daemon is running
2. Opens the specified file using `find-file`
3. Reports success/failure with colored output
4. Shows the buffer name and file path

**Requirements:**
- Emacs daemon must be running (`emacs --daemon`)
- emacsclient must be in PATH

## Testing the New Approach

1. Start fresh:
   ```bash
   cd /home/alex/code/cafedelic/scripts/emacs
   ./init-claude-frame.sh
   ```

2. Open some files:
   ```bash
   ./open-claude-file.sh test-file.md
   ./open-claude-file.sh ../../README.md
   ./open-claude-file.sh ../../package.json
   ```

3. Check status:
   ```bash
   ./show-claude-status.sh
   ```

4. Or run the full test:
   ```bash
   ./test-full-frame.sh
   ```

## Key Improvements

- **No flashing**: Files don't rapidly replace each other
- **Context visibility**: See all recently accessed files at once
- **Simple navigation**: Current file in main area, history above
- **Clean state**: Full frame takeover avoids buffer confusion

## Next Iterations
- [ ] Add buffer positioning (left/right window)
- [ ] Implement buffer naming with prefix
- [ ] Handle multiple file scenarios
- [ ] Add buffer listing functionality
