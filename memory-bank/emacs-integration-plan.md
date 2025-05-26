# Emacs Buffer Management Integration Plan

## Vision

Transform Cafedelic from passive monitoring to active development environment coordination by automatically opening files in Emacs as Claude accesses them. This creates a synchronized view where developers can see exactly what Claude is examining in real-time.

## Architecture Overview

```
DC Logs → Watcher → File Path Extractor → Editor Manager → Emacs Daemon
                            ↓                    ↓
                      Path Cache            Elisp Scripts
                     (Future: SQLite)      (Shell Wrappers)
```

## Core Components

### 1. EditorManager Service

```typescript
class EditorManager extends EventEmitter {
  private fileCache: Set<string> = new Set();
  private emacsApi: EmacsApi;
  
  // Listen for file access events
  onFileAccess(filePath: string) {
    if (!this.fileCache.has(filePath)) {
      this.fileCache.add(filePath);
      this.emacsApi.openFile(filePath);
    }
  }
  
  // Get current editor state
  async getOpenFiles(): Promise<string[]> {
    return this.emacsApi.getAllOpenFiles();
  }
  
  // Focus on specific file
  async focusFile(filePath: string) {
    return this.emacsApi.switchToBuffer(filePath);
  }
}
```

### 2. Emacs API Layer

Shell scripts that execute elisp code and return results:

```
scripts/emacs/
├── open-file.sh          # Opens file in emacs
├── get-open-files.sh     # Lists all open buffers
├── switch-to-buffer.sh   # Focuses specific buffer
├── get-current-file.sh   # Returns currently focused file
└── close-file.sh         # Closes specific buffer
```

Example: `open-file.sh`
```bash
#!/bin/bash
FILE_PATH="$1"
emacsclient --eval "(progn
  (find-file \"$FILE_PATH\")
  (message \"Opened: $FILE_PATH\"))"
```

### 3. File Path Extraction

Enhance existing translator to extract and emit file paths:

```typescript
class TranslatorService {
  translate(entry: DCLogEntry): string {
    // Existing translation logic...
    
    // Extract file paths
    if (entry.command === 'read_file' && entry.args.path) {
      this.emit('file-accessed', entry.args.path);
    }
    
    if (entry.command === 'read_multiple_files' && entry.args.paths) {
      entry.args.paths.forEach(path => {
        this.emit('file-accessed', path);
      });
    }
  }
}
```

### 4. Integration Flow

```
1. DC Log Entry: read_multiple_files
2. Translator: Extracts file paths, emits events
3. EditorManager: Receives file-accessed events
4. Cache Check: Skip if already open
5. Emacs API: Shell script opens file
6. Emacs Daemon: File appears in buffer
```

## Implementation Phases

### Phase 1: Basic File Opening
- Create shell/elisp scripts for basic operations
- Implement EditorManager service
- Wire up file-accessed events
- Test with simple file operations

### Phase 2: Smart Buffer Management
- Track which files are already open
- Implement buffer switching/focusing
- Add file close detection
- Handle multiple file scenarios

### Phase 3: Advanced Features
- Buffer organization (group by project/type)
- Highlight recently accessed sections
- Show access patterns in modeline
- Integrate with projectile/perspective

## Cache Strategy

### In-Memory (Phase 1)
```typescript
interface FileAccessRecord {
  path: string;
  firstAccessed: Date;
  lastAccessed: Date;
  accessCount: number;
  commands: string[]; // Which commands accessed it
}
```

### SQLite Schema (Future)
```sql
CREATE TABLE file_access (
  id INTEGER PRIMARY KEY,
  path TEXT NOT NULL,
  first_accessed TIMESTAMP,
  last_accessed TIMESTAMP,
  access_count INTEGER DEFAULT 1,
  project_path TEXT,
  file_type TEXT
);

CREATE TABLE access_log (
  id INTEGER PRIMARY KEY,
  file_id INTEGER,
  timestamp TIMESTAMP,
  command TEXT,
  context TEXT,
  FOREIGN KEY (file_id) REFERENCES file_access(id)
);
```

## Elisp Scripts Design

### Core Functions Needed

1. **cafedelic-open-file**
   - Open file if not already open
   - Switch to existing buffer if present
   - Return buffer name

2. **cafedelic-get-all-files**
   - List all file-backed buffers
   - Exclude special buffers
   - Return as JSON array

3. **cafedelic-focus-file**
   - Switch to buffer by file path
   - Optionally split window
   - Return success/failure

4. **cafedelic-get-project-files**
   - Group files by project
   - Use projectile if available
   - Return hierarchical structure

## Event Flow Example

```
User: "Claude, analyze the authentication system"

1. [7:10pm] Claude used read_multiple_files
   → Extract: [auth.js, login.js, session.js]
   → EditorManager: Opens all three files
   → Emacs: Three buffers appear

2. [7:10pm] Claude is editing auth.js
   → EditorManager: Focuses auth.js buffer
   → Emacs: Switches to auth.js

3. [7:11pm] Claude searched for "JWT" in src/
   → Future: Highlight search matches in open buffers
```

## Configuration Options

```typescript
interface EditorConfig {
  autoOpen: boolean;          // Automatically open files
  openDelay: number;          // Delay before opening (ms)
  maxOpenFiles: number;       // Limit concurrent buffers
  groupByProject: boolean;    // Use projectile/perspective
  focusOnEdit: boolean;       // Switch to file being edited
  preserveLayout: boolean;    // Maintain window configuration
}
```

## Benefits

1. **Real-time Visibility**: See exactly what Claude is examining
2. **Context Preservation**: Build working set of relevant files
3. **Navigation Aid**: Quick access to Claude-touched files
4. **Pattern Recognition**: Understand file relationships
5. **Debugging Helper**: Trace Claude's exploration path

## Challenges to Address

1. **Performance**: Opening many files quickly
2. **Layout Management**: Preserving user's window setup
3. **File Filtering**: Ignore temporary/generated files
4. **Remote Files**: Handle SSH/TRAMP paths
5. **Large Files**: Async loading for big files

## Success Metrics

- Files open within 100ms of DC log entry
- No disruption to user's current buffer
- Clear indication of Claude-opened files
- Useful file access history
- Smooth integration with existing workflow

## Next Steps

1. Create proof-of-concept elisp functions
2. Build simple shell script wrappers
3. Implement basic EditorManager
4. Test with real DC log data
5. Iterate based on usage patterns
