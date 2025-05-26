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
    (split-window-vertically 10) ; Top window is 10 lines
    
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
  "Add a file to the recent files list and open it"
  (let ((timestamp (current-time-string)))
    ;; Remove if already in list (to move to top)
    (setq cafedelic-recent-files
          (assoc-delete-all filepath cafedelic-recent-files))
    
    ;; Add to front of list
    (push (cons filepath timestamp) cafedelic-recent-files)
    
    ;; Trim list if too long
    (when (> (length cafedelic-recent-files) cafedelic-max-recent-files)
      (setcdr (nthcdr (1- cafedelic-max-recent-files) cafedelic-recent-files) nil))
    
    ;; Open the file in bottom window
    (when (window-live-p (get-buffer-window (current-buffer)))
      (find-file filepath))
    
    ;; Update the files list display
    (cafedelic-update-files-display)))

(defun cafedelic-render-recent-files ()
  "Render the recent files list in the current buffer"
  (insert "═══ Claude's Recent Files ═══════════════════════════════════\n\n")
  
  (if (null cafedelic-recent-files)
      (insert "  No files accessed yet.\n")
    (dolist (entry cafedelic-recent-files)
      (let* ((filepath (car entry))
             (timestamp (cdr entry))
             (filename (file-name-nondirectory filepath))
             (directory (file-name-directory filepath)))
        (insert (format "  %-30s  %s\n" 
                        (truncate-string-to-width filename 30)
                        (cafedelic-format-timestamp timestamp))))))
  
  (insert "\n═════════════════════════════════════════════════════════════"))

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
