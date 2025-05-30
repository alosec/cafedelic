## Claude Code Log Watcher Implementation Plan

Based on analysis of Claude Code's logging system, here's the implementation plan for the first watcher that opens files in Emacs.

### Analysis Findings

**File Access Patterns:**
- Claude Code reads files in bursts, typically 4-6 seconds apart during active exploration
- Longer pauses (10-13 minutes) occur between different task phases
- Multiple files are often accessed sequentially when building context

**Log Structure:**
- Session logs: `~/.claude/projects/-home-alex-code-cafedelic/[session-uuid].jsonl`
- JSONL format with tool_use blocks containing Read operations
- Each Read operation includes timestamp and file_path

### Implementation Design

```
File operation plan:
└── watchers/
    └── claude-code-log.ts [C] - Claude Code log watcher implementation
└── transforms/
    └── claude-code-operations.ts [C] - Transform Claude Code log entries to file operations
└── pipelines/
    └── claude-code-to-emacs.ts [C] - Pipeline to open Claude Code files in Emacs
└── scripts/
    └── emacs/
        └── cafedelic-editor.el [M] - Add read-only mode support for Claude Code files
```

### Key Features

1. **Intelligent Debouncing**
   - Batch file opens within 5-second windows
   - Display "Claude is reviewing N files..." status
   - Sequential opening with 200ms delays to prevent overwhelming Emacs

2. **File Type Filtering**
   - Skip memory-bank files (already tracked separately)
   - Skip binary files and very large files (>1MB)
   - Configurable ignore patterns

3. **Read-Only Protection**
   - All Claude Code files open in read-only mode by default
   - Visual indicator in mode line: `[Claude:RO]`
   - Quick toggle command to make editable if needed

4. **Session Tracking**
   - Monitor only the current active session
   - Auto-detect new sessions and clear previous file list
   - Persistent tail process for real-time monitoring

### Technical Approach

**Watcher Architecture:**
```typescript
// Similar to mcp-log.ts but for Claude Code logs
export async function* watchClaudeCodeLogs(): AsyncGenerator<ClaudeCodeLogEntry> {
  const sessionPath = await findActiveSession();
  const tail = spawn('tail', ['-f', '-n', '0', sessionPath]);
  // Parse JSONL entries and emit Read operations
}
```

**Debouncing Logic:**
```typescript
const fileQueue = new Set<string>();
let debounceTimer: NodeJS.Timeout;

function queueFileOpen(filePath: string) {
  fileQueue.add(filePath);
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(flushFileQueue, 5000);
}
```

**Emacs Integration:**
```elisp
(defun cafedelic-open-claude-file-readonly (filepath)
  "Open file in read-only mode for Claude Code"
  (find-file filepath)
  (read-only-mode 1)
  (setq mode-line-format 
        (append '("[Claude:RO] ") mode-line-format)))
```

### Next Steps

1. Implement claude-code-log.ts watcher
2. Create transform logic for extracting file operations
3. Build pipeline to connect watcher to Emacs
4. Update cafedelic-editor.el with read-only mode support
5. Add configuration for filtering and debouncing

This approach provides visibility into Claude Code's file access patterns while preventing accidental modifications and managing the rapid file access rate intelligently.