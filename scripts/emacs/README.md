# Emacs Integration Scripts

This directory contains shell scripts that interface with Emacs via emacsclient.

## Scripts

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

## Testing Steps

1. Ensure your emacs daemon is running:
   ```bash
   # Check if running
   emacsclient --eval "(emacs-version)"
   
   # If not, start it
   emacs --daemon
   ```

2. Run the basic test:
   ```bash
   cd /home/alex/code/cafedelic/scripts/emacs
   ./open-file-test.sh test-file.md
   ```

3. Test the two-window system:
   ```bash
   # Initialize the layout
   ./init-claude-view.sh
   
   # Open files in Claude's buffer
   ./open-right.sh test-file.md
   ./open-right.sh ../../README.md
   ```

4. Run the complete test suite:
   ```bash
   ./test-window-management.sh
   ```

## Current Behavior

- **init-claude-view.sh**: Creates a split window, preserves your left buffer
- **open-right.sh**: Opens files in right window with `claude-` prefix
- Focus returns to your original window after operations
- Re-opening a file switches to existing buffer instead of creating new one

## Next Iterations
- [ ] Add buffer positioning (left/right window)
- [ ] Implement buffer naming with prefix
- [ ] Handle multiple file scenarios
- [ ] Add buffer listing functionality
