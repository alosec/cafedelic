# Emacs Configuration Extraction Guide

## Files to Copy to ~/.emacs.d

### 1. Core Tree Generation Script
**Source**: `generate-file-tree.sh` (commit 3205f67)
**Destination**: `~/.emacs.d/scripts/generate-file-tree.sh`

```bash
# Extract the script
cd /home/alex/code/cafedelic
git show 3205f67:scripts/generate-file-tree.sh > ~/.emacs.d/scripts/generate-file-tree.sh
chmod +x ~/.emacs.d/scripts/generate-file-tree.sh

# Test it works
echo '["README.md", "src/index.ts", "package.json"]' | ~/.emacs.d/scripts/generate-file-tree.sh --root ~/.emacs.d
```

### 2. Core Elisp Framework
**Source**: `cafedelic-frame.el` (commit b84877d)  
**Destination**: `~/.emacs.d/elisp/file-tree.el` (adapted)

**Key Adaptations Needed**:
- Rename functions: `cafedelic-*` → `file-tree-*`
- Integrate with `perspectives.el` for project detection
- Hook into `find-file-hook` for automatic tracking
- Remove cafedelic-specific configuration

### 3. Integration Test Scripts
**Source**: Various test scripts from commits
**Purpose**: Validate the implementation works

## Quick Extraction Commands

```bash
# Create directories
mkdir -p ~/.emacs.d/scripts
mkdir -p ~/.emacs.d/elisp

# Extract tree generator
cd /home/alex/code/cafedelic
git show 3205f67:scripts/generate-file-tree.sh > ~/.emacs.d/scripts/generate-file-tree.sh
chmod +x ~/.emacs.d/scripts/generate-file-tree.sh

# Extract elisp framework (needs adaptation)
git show b84877d:scripts/emacs/cafedelic-frame.el > ~/.emacs.d/elisp/file-tree-base.el

# Extract documentation
git show b84877d:scripts/emacs/README.md > ~/.emacs.d/docs/file-tree-reference.md
```

## Core Integration Pattern for Perspectives.el

### Essential Hooks
```elisp
;; In your emacs config
(require 'file-tree) ; Your adapted version

;; Hook into perspectives
(add-hook 'persp-created-functions 'file-tree-init-for-project)
(add-hook 'persp-activated-functions 'file-tree-refresh-for-project)

;; Hook into file operations  
(add-hook 'find-file-hook 'file-tree-track-opened-file)
(add-hook 'after-save-hook 'file-tree-update-on-save)
```

### Key Functions to Adapt
```elisp
;; Original cafedelic function
(defun cafedelic-find-project-root (filepath))

;; Should become
(defun file-tree-find-project-root (filepath))
;; Or better: integrate with perspectives current project

;; Original cafedelic function  
(defun cafedelic-init-frame ())

;; Should become
(defun file-tree-init-sidebar ())
;; Designed to work within existing frame layout
```

## Recommended Implementation Sequence

### Phase 1: Basic Tree Display (1-2 hours)
1. Copy and adapt the elisp file
2. Test tree generation script independently  
3. Create simple sidebar display
4. Verify JSON → tree rendering works

### Phase 2: Perspectives Integration (2-3 hours)
1. Hook into perspectives project switching
2. Implement project-specific file tracking
3. Add automatic tree updates on file access
4. Test with multiple projects/perspectives

### Phase 3: Enhanced Features (3-5 hours)
1. Add tree navigation (click to open files)
2. Implement directory folding/expansion
3. Add file type icons (optional)
4. Git status integration (optional)

## Critical Dependencies

### Required System Tools
- `jq` for JSON processing in bash script
- `emacs` with `emacsclient` for integration

### Required Emacs Packages
- `perspectives.el` (already installed)
- Optional: `all-the-icons` for file type icons
- Optional: `magit` for git status integration

## Testing Strategy

### Unit Testing
```bash
# Test tree script independently
echo '["src/main.js", "README.md", "package.json"]' | ~/.emacs.d/scripts/generate-file-tree.sh

# Expected output:
# ├── README.md
# ├── package.json  
# └── src/
#     └── main.js
```

### Integration Testing  
```elisp
;; Test in emacs
(file-tree-init-sidebar)
(file-tree-add-file "/path/to/test/file.js")
;; Verify sidebar shows tree structure
```

## Notes on Original Implementation

### What Worked Well
- **External bash script**: Clean separation of concerns
- **Fixed sidebar width**: Prevents layout issues  
- **Recent files tracking**: Useful for development workflow
- **Project root detection**: Works across different project types

### Potential Improvements
- **Tree navigation**: Original was display-only
- **Performance**: Could cache tree generation for large projects
- **Git integration**: Show file status in tree
- **Customization**: Make colors/icons configurable

## Quick Start for Testing

```bash
# 1. Extract the files
cd /home/alex/code/cafedelic
mkdir -p ~/.emacs.d/scripts
git show 3205f67:scripts/generate-file-tree.sh > ~/.emacs.d/scripts/generate-file-tree.sh
chmod +x ~/.emacs.d/scripts/generate-file-tree.sh

# 2. Test tree generation
cd ~/.emacs.d
echo '["init.el", "elisp/my-config.el", "scripts/generate-file-tree.sh"]' | scripts/generate-file-tree.sh --root ~/.emacs.d

# 3. If that works, proceed with elisp integration
```

This gives you a solid foundation to enhance your emacs configuration with the proven file tree functionality from cafedelic's history.