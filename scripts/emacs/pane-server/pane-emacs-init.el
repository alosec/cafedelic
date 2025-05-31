;; pane-emacs-init.el - Initialization for pane-specific emacs servers
;; This file is loaded when starting emacs in a tmux pane with server mode

;; Track which pane this server belongs to
(defvar cafedelic-pane-id nil
  "The tmux pane ID this emacs server is running in")

;; Recent files tracking (similar to cafedelic-editor.el)
(defvar cafedelic-recent-files '()
  "List of recently accessed files. Each entry is (filepath . timestamp)")

(defvar cafedelic-max-recent-files 50
  "Maximum number of recent files to track")

;; Initialize pane server
(defun cafedelic-pane-init ()
  "Initialize Cafedelic pane server"
  (interactive)
  
  ;; Set frame title to show server name
  (when server-name
    (setq frame-title-format 
          (format "Emacs [%s]" server-name)))
  
  ;; Enable global auto-revert for seamless WTE integration
  ;; This makes the pane "Claude's workspace" where files auto-sync
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil)        ; No "reverted" messages
  (setq auto-revert-check-vc-info t)    ; Also revert version control info
  
  ;; Simple, clean scratch buffer
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (insert (format ";; Cafedelic Pane Server: %s\n" (or server-name "unnamed")))
  (insert ";; Ready for file operations\n")
  (insert ";; Files will open here via emacsclient\n")
  (insert ";; Auto-revert enabled - files sync automatically\n\n")
  
  (message "Cafedelic pane server initialized with auto-revert"))

;; File opening with tracking
(defun cafedelic-pane-open-file (filepath)
  "Open a file and track it as recently accessed"
  (interactive "fFile: ")
  
  ;; Expand to absolute path
  (setq filepath (expand-file-name filepath))
  
  ;; Add to recent files
  (setq cafedelic-recent-files
        (cons (cons filepath (current-time))
              (seq-remove (lambda (entry) (string= (car entry) filepath))
                          cafedelic-recent-files)))
  
  ;; Limit list size
  (when (> (length cafedelic-recent-files) cafedelic-max-recent-files)
    (setq cafedelic-recent-files 
          (seq-take cafedelic-recent-files cafedelic-max-recent-files)))
  
  ;; Open the file in current window
  (find-file filepath)
  
  ;; Return success indicator
  (format "Opened: %s" filepath))

;; File opening in read-only mode (for Claude Code)
(defun cafedelic-pane-open-file-readonly (filepath)
  "Open a file in read-only mode and track it as recently accessed"
  (interactive "fFile: ")
  
  ;; Expand to absolute path
  (setq filepath (expand-file-name filepath))
  
  ;; Add to recent files
  (setq cafedelic-recent-files
        (cons (cons filepath (current-time))
              (seq-remove (lambda (entry) (string= (car entry) filepath))
                          cafedelic-recent-files)))
  
  ;; Limit list size
  (when (> (length cafedelic-recent-files) cafedelic-max-recent-files)
    (setq cafedelic-recent-files 
          (seq-take cafedelic-recent-files cafedelic-max-recent-files)))
  
  ;; Open the file in current window
  (find-file filepath)
  
  ;; Enable read-only mode
  (read-only-mode 1)
  
  ;; Add visual indicator in mode line
  (setq-local mode-line-misc-info 
              (append mode-line-misc-info '(" [Claude:RO]")))
  
  ;; Return success indicator
  (format "Opened read-only: %s" filepath))

;; Directory opening with dired
(defun cafedelic-pane-open-directory (dirpath)
  "Open a directory in dired"
  (interactive "DDirectory: ")
  
  ;; Expand to absolute path
  (setq dirpath (expand-file-name dirpath))
  
  ;; Open in dired
  (dired dirpath)
  
  ;; Return success indicator
  (format "Opened directory: %s" dirpath))

;; Helper to get current buffer info
(defun cafedelic-pane-buffer-info ()
  "Get information about current buffer"
  (list :buffer (buffer-name)
        :file (buffer-file-name)
        :mode major-mode
        :point (point)
        :size (buffer-size)))

;; Initialize on load
(cafedelic-pane-init)

;; Provide the feature
(provide 'cafedelic-pane-emacs)
