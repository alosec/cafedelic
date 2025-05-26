# Emacs Integration Scripts

This directory contains shell scripts that interface with Emacs via emacsclient.

## Current Approach: Full Frame UI

After testing split-window approaches, we've moved to a full-frame UI that better visualizes Claude's "context window":

```
═══ Claude's Recent Files ═══════════════════════════════════
  test-file.md                    12:34:56
  README.md                       12:34:52  
  package.json                    12:34:48
═════════════════════════════════════════════════════════════
[Current file content shown below]
```

## New Full Frame Scripts

### cafedelic-frame.el
Core elisp functions for managing the full frame UI and recent files list.

### init-claude-frame.sh  
Initializes the full frame layout with recent files list at top.

**Usage:**
```bash
./init-claude-frame.sh
```

### open-claude-file.sh
Opens a file and adds it to the recent files list.

**Usage:**
```bash
./open-claude-file.sh <file-path>
```

### show-claude-status.sh
Displays current Cafedelic state and recent files.

**Usage:**
```bash
./show-claude-status.sh
```

### test-full-frame.sh
Demonstrates the full frame workflow.

**Usage:**
```bash
./test-full-frame.sh
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
