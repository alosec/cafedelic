;; cafedelic-editor.el - Simple editor configuration for Cafedelic
;; Focuses only on editing capabilities, file tree is in tmux pane 1

(defvar cafedelic-recent-files '()
  "List of recently accessed files. Each entry is (filepath . timestamp)")

(defvar cafedelic-max-recent-files 50
  "Maximum number of recent files to track")

(defun cafedelic-init-editor ()
  "Initialize Cafedelic editor - simple and clean"
  (interactive)
  
  ;; Clean up any existing state
  (cafedelic-cleanup-state)
  
  ;; Delete all other windows for clean slate
  (delete-other-windows)
  
  ;; Show scratch buffer initially
  (switch-to-buffer "*scratch*")
  (insert ";; Cafedelic Editor Ready\n")
  (insert ";; File tree is in the adjacent tmux pane\n")
  (insert ";; Files will open here automatically\n")
  
  "Cafedelic editor initialized")

(defun cafedelic-cleanup-state ()
  "Clean up any leftover buffers"
  (interactive)
  ;; Kill any existing cafedelic buffers
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*Claude" (buffer-name buffer))
      (kill-buffer buffer))))

(defun cafedelic-open-file (filepath)
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
  
  ;; Open the file
  (find-file filepath)
  
  ;; Log the action
  (message "Opened: %s" filepath))

;; Initialize on load
(cafedelic-init-editor)

;; Provide the feature
(provide 'cafedelic-editor)
