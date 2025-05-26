;; cafedelic-frame.el - Fixed version with left sidebar tree
;; Uses external generate-file-tree.sh for tree rendering

(defvar cafedelic-recent-files '()
  "List of recently accessed files. Each entry is (filepath . timestamp)")

(defvar cafedelic-max-recent-files 50
  "Maximum number of recent files to track")

(defvar cafedelic-tree-buffer-name "*Claude-Tree*"
  "Name of the buffer showing file tree")

(defvar cafedelic-tree-width 30
  "Width of the tree sidebar")

(defvar cafedelic-project-root nil
  "Project root for tree display")

(defun cafedelic-init-frame ()
  "Initialize Cafedelic frame with left tree sidebar and right content area"
  (interactive)
  
  ;; Clean up any existing cafedelic state
  (cafedelic-cleanup-state)
  
  ;; Delete ALL other windows for truly clean slate
  (delete-other-windows)
  
  ;; Create tree buffer
  (let ((tree-buffer (get-buffer-create cafedelic-tree-buffer-name)))
    ;; Split window vertically (left/right)
    (split-window-horizontally cafedelic-tree-width)
    
    ;; Left window: tree
    (set-window-buffer (selected-window) tree-buffer)
    (with-current-buffer tree-buffer
      (read-only-mode 0)
      (erase-buffer)
      (insert "No files accessed yet")
      (read-only-mode 1)
      ;; Lock window width
      (setq-local window-size-fixed 'width))
    
    ;; Move to right window for content
    (other-window 1)
    
    ;; Try to detect project root
    (when buffer-file-name
      (setq cafedelic-project-root 
            (cafedelic-find-project-root buffer-file-name)))
    
    "Cafedelic frame initialized"))

(defun cafedelic-cleanup-state ()
  "Clean up any leftover buffers from previous approaches"
  ;; Kill old claude-* buffers
  (dolist (buf (buffer-list))
    (when (string-prefix-p "claude-" (buffer-name buf))
      (kill-buffer buf)))
  ;; Kill old file list buffer if exists
  (when (get-buffer "*Claude-Files*")
    (kill-buffer "*Claude-Files*")))

(defun cafedelic-add-file (filepath)
  "Add a file to recent list and display it"
  (let ((timestamp (current-time-string)))
    ;; Update project root if needed
    (unless cafedelic-project-root
      (setq cafedelic-project-root (cafedelic-find-project-root filepath)))
    
    ;; Add to recent files
    (setq cafedelic-recent-files
          (assoc-delete-all filepath cafedelic-recent-files))
    (push (cons filepath timestamp) cafedelic-recent-files)
    
    ;; Trim list
    (when (> (length cafedelic-recent-files) cafedelic-max-recent-files)
      (setcdr (nthcdr (1- cafedelic-max-recent-files) cafedelic-recent-files) nil))
    
    ;; Update tree display
    (cafedelic-update-tree-display)
    
    ;; Open file in right window
    (let ((content-window (cafedelic-get-content-window)))
      (when content-window
        (select-window content-window)
        (find-file filepath)))
    
    filepath))

(defun cafedelic-get-content-window ()
  "Get the content window (right side)"
  (let ((tree-window (get-buffer-window cafedelic-tree-buffer-name)))
    (when tree-window
      ;; Get the window to the right of tree
      (window-in-direction 'right tree-window))))

(defun cafedelic-update-tree-display ()
  "Update the tree display using generate-file-tree.sh"
  (when-let ((tree-buffer (get-buffer cafedelic-tree-buffer-name)))
    (with-current-buffer tree-buffer
      (read-only-mode 0)
      (erase-buffer)
      
      (if (null cafedelic-recent-files)
          (insert "No files accessed yet")
        ;; Generate file list JSON
        (let* ((files (mapcar #'car cafedelic-recent-files))
               (json-files (json-encode files))
               (script-path (expand-file-name 
                             "scripts/generate-file-tree.sh"
                             (or cafedelic-project-root default-directory)))
               (tree-output))
          
          ;; Call the tree script
          (if (and cafedelic-project-root (file-exists-p script-path))
              (let ((default-directory cafedelic-project-root))
                (setq tree-output
                      (shell-command-to-string
                       (format "echo '%s' | %s --root %s"
                               json-files
                               script-path
                               cafedelic-project-root))))
            ;; Fallback to simple list
            (setq tree-output
                  (mapconcat 
                   (lambda (f) (format "• %s" (file-name-nondirectory f)))
                   files
                   "\n")))
          
          (insert tree-output)))
      
      (read-only-mode 1))))

(defun cafedelic-find-project-root (filepath)
  "Find project root by looking for markers"
  (let ((dir (file-name-directory (expand-file-name filepath)))
        (markers '(".git" "package.json" "Cargo.toml" "go.mod")))
    (catch 'found
      (while (and dir (not (string= dir "/")))
        (dolist (marker markers)
          (when (file-exists-p (expand-file-name marker dir))
            (throw 'found dir)))
        (setq dir (file-name-directory (directory-file-name dir))))
      nil)))

(defun cafedelic-clear-recent-files ()
  "Clear the recent files list"
  (interactive)
  (setq cafedelic-recent-files '())
  (cafedelic-update-tree-display)
  "Recent files cleared")

(provide 'cafedelic-frame)