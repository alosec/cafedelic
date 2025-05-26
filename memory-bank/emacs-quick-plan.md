# Emacs Integration - Quick Planning Summary

## Goal
Automatically open files in Emacs as Claude accesses them, creating a synchronized development view.

## Key Insight from Current Success
```
[7:10pm] Claude used read_multiple_files
         ↓
    Extract paths: [auth.js, login.js, session.js]
         ↓
    Open in Emacs automatically
```

## Proposed Architecture

### 1. EditorManager Service
- Listens for file-accessed events from TranslatorService
- Maintains cache of opened files
- Calls Emacs via shell scripts

### 2. Shell/Elisp Bridge
```
scripts/emacs/
├── open-file.sh       # emacsclient --eval "(find-file \"$1\")"
├── get-open-files.sh  # List all file buffers as JSON
└── focus-file.sh      # Switch to specific buffer
```

### 3. Enhanced Translation
```typescript
// In TranslatorService
if (entry.command === 'read_file') {
  this.emit('file-accessed', entry.args.path);
}
if (entry.command === 'read_multiple_files') {
  entry.args.paths?.forEach(path => 
    this.emit('file-accessed', path)
  );
}
```

## Implementation Priority
1. **Basic**: Open single files when accessed
2. **Better**: Handle multiple files, track what's open
3. **Best**: Smart buffer management, project grouping

## Why This Matters
- See Claude's exploration in real-time
- Build context automatically
- Never lose track of relevant files
- Prepare for future SQLite integration

## Next Actions
1. Create proof-of-concept elisp script
2. Test shell script integration
3. Add EditorManager service
4. Wire up events
