;; cafedelic-frame.el - Core elisp functions for Cafedelic full frame UI
;; Manages recent files list and frame layout

(defvar cafedelic-recent-files '()
  "List of recently accessed files. Each entry is (filepath . timestamp)")

(defvar cafedelic-max-recent-files 20
  "Maximum number of recent files to display")

(defvar cafedelic-files-buffer-name "*Claude-Files*"
  "Name of the buffer showing recent files")

(defun cafedelic-init-frame ()
  "Initialize Cafedelic frame with recent files list and content area"
  (interactive)
  ;; Delete other windows for clean slate
  (delete-other-windows)
  
  ;; Create recent files buffer
  (let ((files-buffer (get-buffer-create cafedelic-files-buffer-name)))
    ;; Split window horizontally (top/bottom)
    (split-window-vertically 8) ; Reduced from 10 to 8 lines
    
    ;; Setup top window with files list
    (set-window-buffer (selected-window) files-buffer)
    (with-current-buffer files-buffer
      (read-only-mode 0)
      (erase-buffer)
      (cafedelic-render-recent-files)
      (read-only-mode 1))
    
    ;; Move to bottom window
    (other-window 1)
    
    ;; Return success message
    "Cafedelic frame initialized"))

(defun cafedelic-add-file (filepath)
  "Add a file to the recent files list and open it immediately"
  (let ((timestamp (current-time-string)))
    ;; Remove if already in list (to move to top)
    (setq cafedelic-recent-files
          (assoc-delete-all filepath cafedelic-recent-files))
    
    ;; Add to front of list
    (push (cons filepath timestamp) cafedelic-recent-files)
    
    ;; Trim list if too long
    (when (> (length cafedelic-recent-files) cafedelic-max-recent-files)
      (setcdr (nthcdr (1- cafedelic-max-recent-files) cafedelic-recent-files) nil))
    
    ;; Update the files list display first
    (cafedelic-update-files-display)
    
    ;; Then open the file in bottom window (creates flash effect)
    (save-selected-window
      ;; Find the bottom window
      (select-window (or (window-in-direction 'below)
                         (split-window-vertically)))
      ;; Open the file
      (find-file filepath))
    
    ;; Return the filepath
    filepath))

(defun cafedelic-render-recent-files ()
  "Render the recent files as a minimal tree structure"
  (if (null cafedelic-recent-files)
      (insert "No files accessed yet.\n")
    ;; Build and insert the tree
    (let ((tree-lines (cafedelic-build-file-tree)))
      (dolist (line tree-lines)
        (insert line "\n")))))

(defun cafedelic-build-file-tree ()
  "Build a tree structure from recent files list"
  (let ((tree-map (make-hash-table :test 'equal))
        (root-files '())
        (result '()))
    
    ;; First pass: organize files by directory
    (dolist (entry cafedelic-recent-files)
      (let* ((filepath (car entry))
             (project-relative (cafedelic-make-project-relative filepath))
             (parts (split-string project-relative "/" t))
             (filename (car (last parts))))
        
        (if (= (length parts) 1)
            ;; Root level file
            (push filename root-files)
          ;; File in subdirectory
          (let ((dir-path (mapconcat 'identity (butlast parts) "/")))
            (push filename (gethash dir-path tree-map '()))))))
    
    ;; Second pass: build the tree lines
    ;; Add root files first
    (dolist (file (reverse root-files))
      (push (concat "├── " file) result))
    
    ;; Add directories and their files
    (let ((dirs (sort (hash-table-keys tree-map) 'string<)))
      (dolist (dir dirs)
        (let* ((parts (split-string dir "/" t))
               (indent (make-string (* 4 (1- (length parts))) ?\s))
               (parent-indent (if (> (length parts) 1)
                                  (make-string (* 4 (- (length parts) 2)) ?\s)
                                ""))
               (dir-name (car (last parts)))
               (files (reverse (gethash dir tree-map))))
          
          ;; Add directory line
          (push (concat parent-indent "├── " dir-name "/") result)
          
          ;; Add files in directory
          (dolist (file files)
            (push (concat indent "├── " file) result)))))
    
    ;; Fix last item markers
    (when result
      (let ((last-line (car (last result))))
        (setcar (last result) 
                (replace-regexp-in-string "├──" "└──" last-line))))
    
    (reverse result)))

(defun cafedelic-make-project-relative (filepath)
  "Make filepath relative to project root"
  ;; Simple heuristic: find common project markers
  (let ((project-root (cafedelic-find-project-root filepath)))
    (if project-root
        (file-relative-name filepath project-root)
      (file-name-nondirectory filepath))))

(defun cafedelic-find-project-root (filepath)
  "Find project root by looking for markers like .git, package.json"
  (let ((dir (file-name-directory filepath))
        (markers '(".git" "package.json" "Cargo.toml" "go.mod")))
    (catch 'found
      (while (and dir (not (string= dir "/")))
        (dolist (marker markers)
          (when (file-exists-p (expand-file-name marker dir))
            (throw 'found dir)))
        (setq dir (file-name-directory (directory-file-name dir))))
      nil)))

(defun cafedelic-format-timestamp (timestamp)
  "Format timestamp to show relative time"
  ;; For now, just show the time portion
  ;; TODO: Make this show "2s ago", "5m ago" etc
  (substring timestamp 11 19))

(defun cafedelic-update-files-display ()
  "Update the recent files display"
  (when-let ((files-buffer (get-buffer cafedelic-files-buffer-name))
             (files-window (get-buffer-window files-buffer)))
    (with-current-buffer files-buffer
      (read-only-mode 0)
      (erase-buffer)
      (cafedelic-render-recent-files)
      (read-only-mode 1))))

(defun cafedelic-clear-recent-files ()
  "Clear the recent files list"
  (interactive)
  (setq cafedelic-recent-files '())
  (cafedelic-update-files-display)
  "Recent files cleared")

(provide 'cafedelic-frame)
