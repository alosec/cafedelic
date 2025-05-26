#!/bin/bash
# clear-cafedelic-state.sh - Clear all Cafedelic state in emacs

YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

echo -e "${YELLOW}Clearing Cafedelic state...${NC}"

# Clear recent files and kill buffers
emacsclient --eval "
(progn
  ;; Clear recent files
  (when (boundp 'cafedelic-recent-files)
    (setq cafedelic-recent-files nil))
  
  ;; Kill tree buffer
  (when (get-buffer \"*Claude-Tree*\")
    (kill-buffer \"*Claude-Tree*\"))
  
  ;; Kill old buffers
  (when (get-buffer \"*Claude-Files*\")
    (kill-buffer \"*Claude-Files*\"))
  (when (get-buffer \"*claude-workspace*\")
    (kill-buffer \"*claude-workspace*\"))
  
  ;; Kill any claude- prefixed buffers
  (dolist (buf (buffer-list))
    (when (string-prefix-p \"claude-\" (buffer-name buf))
      (kill-buffer buf)))
  
  \"State cleared\")" 2>/dev/null

echo -e "${GREEN}Cafedelic state cleared${NC}"
echo "You can now run init-claude-frame.sh for a fresh start"
