#!/bin/bash
# daemon-health.sh - Comprehensive Emacs daemon health check

SOCKET_NAME="${1:-}"
JSON_OUTPUT="${2:-}"

# Colors (disabled for JSON output)
if [ "$JSON_OUTPUT" != "json" ]; then
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    RED='\033[0;31m'
    BLUE='\033[0;34m'
    NC='\033[0m'
else
    GREEN=''
    YELLOW=''
    RED=''
    BLUE=''
    NC=''
fi

# Initialize result
HEALTHY=false
RESPONSIVE=false
PID=""
VERSION=""
UPTIME=""
SOCKET_PATH=""
ERROR=""

# Find socket if not specified
if [ -z "$SOCKET_NAME" ]; then
    SOCKET_PATH=$("$(dirname "$0")/detect-socket.sh" 2>/dev/null)
    if [ -n "$SOCKET_PATH" ]; then
        SOCKET_NAME=$(basename "$SOCKET_PATH")
    fi
else
    # Try to find socket path for given name
    SOCKET_PATH=$("$(dirname "$0")/detect-socket.sh" "$SOCKET_NAME" 2>/dev/null)
fi

# Check if we have a socket
if [ -z "$SOCKET_PATH" ] && [ -z "$SOCKET_NAME" ]; then
    ERROR="No Emacs daemon found"
else
    # Test basic connectivity
    if emacsclient ${SOCKET_NAME:+--socket-name="$SOCKET_NAME"} --eval "t" >/dev/null 2>&1; then
        RESPONSIVE=true
        
        # Get PID
        PID=$(emacsclient ${SOCKET_NAME:+--socket-name="$SOCKET_NAME"} \
              --eval "(emacs-pid)" 2>/dev/null | tr -d '"')
        
        # Get version
        VERSION=$(emacsclient ${SOCKET_NAME:+--socket-name="$SOCKET_NAME"} \
                  --eval "(emacs-version)" 2>/dev/null | tr -d '"' | head -1)
        
        # Get uptime in seconds
        UPTIME=$(emacsclient ${SOCKET_NAME:+--socket-name="$SOCKET_NAME"} \
                 --eval "(floor (float-time (time-subtract (current-time) before-init-time)))" \
                 2>/dev/null | tr -d '"')
        
        # Verify all health indicators
        if [ -n "$PID" ] && [ -n "$VERSION" ] && [ -n "$UPTIME" ]; then
            HEALTHY=true
        else
            ERROR="Incomplete daemon response"
        fi
    else
        ERROR="Daemon not responding"
    fi
fi

# Output results
if [ "$JSON_OUTPUT" = "json" ]; then
    # JSON output
    cat <<EOF
{
  "healthy": $([[ "$HEALTHY" = "true" ]] && echo "true" || echo "false"),
  "responsive": $([[ "$RESPONSIVE" = "true" ]] && echo "true" || echo "false"),
  "pid": ${PID:-null},
  "version": ${VERSION:+"\"$VERSION\""},
  "uptime": ${UPTIME:-null},
  "socketName": ${SOCKET_NAME:+"\"$SOCKET_NAME\""},
  "socketPath": ${SOCKET_PATH:+"\"$SOCKET_PATH\""},
  "error": ${ERROR:+"\"$ERROR\""}
}
EOF
else
    # Human-readable output
    echo -e "${BLUE}=== Emacs Daemon Health Check ===${NC}"
    echo
    
    if [ "$HEALTHY" = "true" ]; then
        echo -e "Status:      ${GREEN}HEALTHY${NC}"
        echo -e "PID:         $PID"
        echo -e "Socket:      ${SOCKET_NAME:-default}"
        [ -n "$SOCKET_PATH" ] && echo -e "Path:        $SOCKET_PATH"
        echo -e "Version:     $VERSION"
        
        # Format uptime
        if [ -n "$UPTIME" ]; then
            HOURS=$((UPTIME / 3600))
            MINUTES=$(((UPTIME % 3600) / 60))
            SECONDS=$((UPTIME % 60))
            echo -e "Uptime:      ${HOURS}h ${MINUTES}m ${SECONDS}s"
        fi
    else
        echo -e "Status:      ${RED}UNHEALTHY${NC}"
        [ -n "$ERROR" ] && echo -e "Error:       $ERROR"
        [ "$RESPONSIVE" = "true" ] && echo -e "Responsive:  ${YELLOW}Partial${NC}"
    fi
fi

# Exit code based on health
[ "$HEALTHY" = "true" ] && exit 0 || exit 1