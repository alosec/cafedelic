#!/bin/bash
# diagnose-emacs.sh - Check current emacs state

CYAN='\033[0;36m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${CYAN}═══ Emacs State Diagnostics ═══${NC}\n"

# Window configuration
echo -e "${YELLOW}Window Configuration:${NC}"
emacsclient --eval "
(let ((windows (window-list)))
  (format \"Total windows: %d\\n%s\"
          (length windows)
          (mapconcat
           (lambda (w)
             (format \"  Window: %s (width: %d)\"
                     (buffer-name (window-buffer w))
                     (window-width w)))
           windows
           \"\\n\")))" 2>/dev/null | tr -d '"' | sed 's/\\n/\n/g'

echo -e "\n${YELLOW}All Buffers:${NC}"
emacsclient --eval "
(mapconcat
 (lambda (b)
   (format \"  %s\" (buffer-name b)))
 (buffer-list)
 \"\\n\")" 2>/dev/null | tr -d '"' | sed 's/\\n/\n/g'

echo -e "\n${YELLOW}Claude-related Buffers:${NC}"
emacsclient --eval "
(let ((claude-buffers
       (seq-filter
        (lambda (b)
          (or (string-prefix-p \"claude-\" (buffer-name b))
              (string= (buffer-name b) \"*Claude-Tree*\")
              (string= (buffer-name b) \"*Claude-Files*\")))
        (buffer-list))))
  (if claude-buffers
      (mapconcat #'buffer-name claude-buffers \"\\n\")
    \"  None found\"))" 2>/dev/null | tr -d '"' | sed 's/\\n/\n/g'

echo -e "\n${YELLOW}Tree Buffer Content:${NC}"
emacsclient --eval "
(if-let ((tree-buffer (get-buffer \"*Claude-Tree*\")))
    (with-current-buffer tree-buffer
      (buffer-string))
  \"Tree buffer not found\")" 2>/dev/null | tr -d '"' | sed 's/\\n/\n/g'

echo -e "\n${YELLOW}Recent Files List:${NC}"
emacsclient --eval "
(if (boundp 'cafedelic-recent-files)
    (if cafedelic-recent-files
        (mapconcat 
         (lambda (entry)
           (format \"  %s\" (car entry)))
         cafedelic-recent-files
         \"\\n\")
      \"  No recent files\")
  \"  Variable not defined\")" 2>/dev/null | tr -d '"' | sed 's/\\n/\n/g'

echo -e "\n${YELLOW}Project Root:${NC}"
emacsclient --eval "
(if (boundp 'cafedelic-project-root)
    cafedelic-project-root
  \"Not set\")" 2>/dev/null | tr -d '"'
