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
  
  ;; Simple, clean scratch buffer
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (insert (format ";; Cafedelic Pane Server: %s\n" (or server-name "unnamed")))
  (insert ";; Ready for file operations\n")
  (insert ";; Files will open here via emacsclient\n\n")
  
  (message "Cafedelic pane server initialized"))

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
