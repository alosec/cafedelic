# QUICK START: Extract Cafedelic File Tree for Emacs

## TL;DR - Copy These Commands

```bash
# 1. Create directories in your emacs config
mkdir -p ~/.emacs.d/scripts ~/.emacs.d/elisp ~/.emacs.d/docs

# 2. Extract the proven file tree generator
cd /home/alex/code/cafedelic
git show 3205f67:scripts/generate-file-tree.sh > ~/.emacs.d/scripts/generate-file-tree.sh
chmod +x ~/.emacs.d/scripts/generate-file-tree.sh

# 3. Extract the elisp framework (needs adaptation)
git show b84877d:scripts/emacs/cafedelic-frame.el > ~/.emacs.d/elisp/file-tree-base.el

# 4. Extract documentation for reference
git show b84877d:scripts/emacs/README.md > ~/.emacs.d/docs/cafedelic-file-tree-reference.md

# 5. Test the tree generator works
echo '["init.el", "elisp/config.el", "scripts/test.sh"]' | ~/.emacs.d/scripts/generate-file-tree.sh --root ~/.emacs.d
```

## What You Get

### 1. **generate-file-tree.sh** - Proven Tree Generator
- **Input**: JSON array of file paths
- **Output**: Beautiful ASCII tree structure
- **Features**: Directory sorting, Unicode characters, project root support
- **Status**: ✅ Ready to use as-is

### 2. **cafedelic-frame.el** - Elisp Framework 
- **Purpose**: Left sidebar file tree with auto-updates
- **Features**: Project detection, recent files tracking, window management
- **Status**: 🔧 Needs adaptation for perspectives.el

### 3. **Documentation** - Implementation Reference
- **Architecture**: How the system works
- **Integration**: How it hooks into emacs
- **Patterns**: Proven approaches that work

## Key Discoveries from Git History

### Architecture Pattern: Two-Layer System
1. **Elisp Layer**: Window management, UI, event handling
2. **Bash Layer**: Tree generation using jq and shell tools

### Why This Works Better Than Pure Elisp
- **Performance**: Shell tools handle tree generation efficiently
- **Maintainability**: Easier to debug and modify bash scripts
- **Reusability**: Tree script can be used by other tools
- **Robustness**: Proven external tools (jq, sort) handle edge cases

### Project Root Detection (Works Great)
```elisp
;; Auto-detects project root by looking for:
'(".git" "package.json" "Cargo.toml" "go.mod")
```

### Window Management (Stable Layout)
- **30-character left sidebar** (can be adjusted)
- **Fixed width** prevents accidental resizing
- **Proper cleanup** prevents buffer accumulation

## Integration Points for Your Setup

### With Perspectives.el
```elisp
;; Hook into perspectives lifecycle
(add-hook 'persp-created-functions 'file-tree-init-for-project)
(add-hook 'persp-activated-functions 'file-tree-switch-project)

;; Per-project file tracking
(defvar persp-file-trees (make-hash-table)) ; project -> files mapping
```

### With Claude Code/eat-mode
```elisp
;; Monitor Claude's file accesses
(add-hook 'eat-mode-hook 'file-tree-setup-claude-monitoring)
;; Parse Claude output for file operations
;; Auto-update tree when Claude opens files
```

## What Needs Adaptation

### Function Renaming
- `cafedelic-*` → `file-tree-*` or `persp-file-tree-*`
- Remove cafedelic-specific configuration

### Perspectives Integration
- Replace custom project detection with perspectives project detection
- Add per-project state management
- Handle perspective switching

### Claude Code Integration
- Add hooks to monitor eat-mode buffer
- Parse Claude output for file access patterns
- Auto-update tree when Claude opens files

## Timeline to Working Implementation

### Phase 1: Basic Tree (1-2 hours)
1. ✅ Extract files (done above)
2. Adapt elisp for generic use
3. Test tree generation
4. Create basic sidebar

### Phase 2: Perspectives Integration (2-3 hours)
1. Hook into perspectives
2. Per-project file tracking
3. State management
4. Perspective switching

### Phase 3: Claude Integration (1-2 hours)
1. Monitor eat-mode
2. Parse Claude output
3. Auto-update trees

### Total: 4-7 hours to full implementation

## Test Commands

```bash
# Test tree generation script
echo '["src/main.js", "README.md", "package.json", "test/spec.js"]' | ~/.emacs.d/scripts/generate-file-tree.sh

# Expected output:
# ├── README.md
# ├── package.json
# ├── src/
# │   └── main.js
# └── test/
#     └── spec.js

# Test with project root
echo '["src/main.js", "README.md"]' | ~/.emacs.d/scripts/generate-file-tree.sh --root /home/alex/project
```

## Key Files in Cafedelic Git History

| Commit | File | Purpose |
|--------|------|---------|
| `3205f67` | `scripts/generate-file-tree.sh` | ✅ Tree generator (ready to use) |
| `b84877d` | `scripts/emacs/cafedelic-frame.el` | 🔧 Elisp framework (needs adaptation) |
| `b84877d` | `scripts/emacs/README.md` | 📖 Usage documentation |
| `31b6139` | `src/services/emacs.service.ts` | 💡 Advanced integration patterns |

## Success Indicators

✅ **Tree script generates proper ASCII trees**  
✅ **Elisp creates stable left sidebar**  
✅ **Trees update when files are opened**  
✅ **Different trees for different perspectives**  
✅ **Claude activity automatically updates trees**

## Next Steps

1. Run the extraction commands above
2. Test the tree generator script  
3. Start adapting the elisp for perspectives
4. Refer to the detailed documentation in `docs/perspectives-file-tree-implementation-plan.md`

This gives you everything you need to implement sophisticated file trees in your emacs configuration using proven, battle-tested code from cafedelic's development.