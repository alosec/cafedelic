# Tmux Pane Customization for Agent Contexts

## Overview

Tmux offers incredible customization options for pane borders and displays. When combined with Cafedelic's agent context system, these features can create powerful visual indicators and functional enhancements for multi-agent development workflows.

## Core Variables Available

### Basic Pane Information
- `#{pane_index}` - Numeric index of the pane
- `#{pane_id}` - Unique pane identifier (e.g., %0, %1)
- `#{pane_title}` - Pane title (set via escape sequences)
- `#{pane_current_command}` - Currently running command
- `#{pane_current_path}` - Current working directory
- `#{pane_pid}` - Process ID of the pane's command
- `#{pane_tty}` - Pseudo-terminal device (e.g., /dev/pts/0)

### Pane Dimensions
- `#{pane_width}` - Width in characters
- `#{pane_height}` - Height in characters
- `#{pane_at_top}`, `#{pane_at_bottom}`, `#{pane_at_left}`, `#{pane_at_right}` - Position indicators

### Pane State
- `#{pane_active}` - 1 if pane is active
- `#{pane_synchronized}` - 1 if pane is synchronized
- `#{pane_in_mode}` - 1 if pane is in copy mode
- `#{pane_input_off}` - 1 if input is disabled
- `#{pane_dead}` - 1 if pane is dead

### Custom Variables (User Options)
- `#{@pane_name}` - Our custom pane name (used by Cafedelic)
- `#{@context_name}` - Could store agent context
- `#{@pane_type}` - Could store pane type (terminal, editor, logs)

## Formatting Options

### Colors
```bash
#[fg=colour123]      # Foreground color
#[bg=colour234]      # Background color
#[fg=red,bg=blue]    # Combined colors
```

### Text Styling
```bash
#[bold]              # Bold text
#[italics]           # Italic text
#[underscore]        # Underlined text
#[reverse]           # Reverse video
```

### Conditional Formatting
```bash
#{?condition,true_value,false_value}
#{?pane_active,#[fg=green]ACTIVE,#[fg=grey]inactive}
```

## Agent Context Integration Ideas

### 1. Show Agent Context in Border
```bash
# Display agent name and pane type
set -g pane-border-format "#{pane_index}: #{?@context_name,Agent: #{@context_name} (#{@pane_type}),#{pane_current_command}}"
```

### 2. Color-Coded Agent Workspaces
```bash
# Different colors for different agents
set -g pane-border-format "#[fg=#{?#{==:#{@context_name},luc},blue,#{?#{==:#{@context_name},pierre},green,#{?#{==:#{@context_name},marie},yellow,white}}]}#{@context_name}: #{@pane_type}"
```

### 3. Activity Indicators
```bash
# Show if agent is actively working
set -g pane-border-format "#{@context_name}: #{@pane_type} #{?#{>:#{pane_pid_runtime},60},🔥 ACTIVE,💤 idle}"
```

## Advanced Scriptable Examples

### 1. Git-Aware Agent Panes
```bash
# Show git branch for editor panes
set -g pane-border-format "#{@context_name}: #{?#{==:#{@pane_type},editor},#(cd #{pane_current_path} && git branch --show-current 2>/dev/null || echo 'no-git'),#{@pane_type}}"
```

### 2. File Count in Editor Panes
```bash
# Show number of open files in Emacs
set -g pane-border-format "#{@context_name}: #{?#{==:#{@pane_type},editor},📝 #(emacsclient --eval '(length (buffer-list))' 2>/dev/null || echo '0') files,#{@pane_type}}"
```

### 3. Resource Usage per Agent
```bash
# Show CPU usage for agent's processes
set -g pane-border-format "#{@context_name}: CPU #(ps -p #{pane_pid} -o %cpu | tail -1)%"
```

### 4. Activity Summary
```bash
# Show last activity from Cafedelic database
set -g pane-border-format "#{@context_name}: #(sqlite3 ~/code/cafedelic/cafedelic.db 'SELECT substr(details,1,20) FROM context_activity_log WHERE context_id=(SELECT id FROM agent_contexts WHERE name=\"#{@context_name}\") ORDER BY timestamp DESC LIMIT 1' 2>/dev/null || echo 'no activity')"
```

### 5. Time-Based Context Indicators
```bash
# Show work hours indicator
set -g pane-border-format "#{@context_name}: #(hour=$(date +%H); if [ $hour -ge 9 -a $hour -lt 17 ]; then echo '🏢 work'; else echo '🏠 after-hours'; fi) #{@pane_type}"
```

## TTY Magic

The `#{pane_tty}` variable opens interesting possibilities:

### Direct Communication
```bash
# Send messages directly to agent panes
echo "Build complete!" > /dev/pts/2

# Stream logs to specific pane
tail -f app.log > /dev/pts/3
```

### Agent Coordination Script
```bash
#!/bin/bash
# Send notifications to all panes of an agent
for pane in $(tmux list-panes -F '#{pane_tty} #{@context_name}' | grep "luc" | cut -d' ' -f1); do
    echo "🔔 Luc: New task assigned" > $pane
done
```

## Implementation Strategies

### 1. Set Context Metadata
When assigning panes to contexts, also set metadata:
```bash
tmux set -t %0 @context_name "luc"
tmux set -t %0 @pane_type "editor"
tmux set -t %0 @context_color "blue"
```

### 2. Dynamic Border Updates
Create a script that updates borders based on activity:
```bash
#!/bin/bash
# Update pane border when files are accessed
tmux set -t $PANE_ID @last_file "$(basename $FILE_PATH)"
tmux refresh-client
```

### 3. Status Line Integration
Use tmux status line to show aggregate agent information:
```bash
set -g status-right "#[fg=blue]Luc: #{@luc_files} files #[fg=green]Pierre: #{@pierre_tasks} tasks"
```

## Cafedelic Integration Opportunities

### 1. Real-time Activity Display
- Show last accessed file in editor pane borders
- Display command count in terminal pane borders
- Show error count in logs pane borders

### 2. Visual Agent States
- 🟢 Active (recent activity)
- 🟡 Idle (no activity for 5 min)
- 🔴 Error state
- 🔵 Waiting for input

### 3. Context Switching Indicators
```bash
# Highlight active context
set -g pane-border-format "#{?#{==:#{@context_name},#{@active_context}},#[fg=green]▶ ,  }#{@context_name}: #{@pane_type}"
```

### 4. Performance Metrics
- Show token usage per agent
- Display file modification rate
- Track command execution time

## Example Complete Configuration

```bash
# Set pane border format with agent context awareness
set -g pane-border-format "#[fg=colour240]#{pane_index}:#[fg=default] \
#{?@context_name,\
  #{?#{==:#{@context_name},luc},#[fg=blue],\
  #{?#{==:#{@context_name},pierre},#[fg=green],\
  #{?#{==:#{@context_name},marie},#[fg=yellow],\
  #[fg=white]}}}Agent #{@context_name}#[fg=default] │ ,\
  No context │ }\
#{?@pane_type,#[fg=cyan]#{@pane_type}#[fg=default],#{pane_current_command}} \
#{?#{==:#{@pane_type},editor},\
  📝 #(cd #{pane_current_path} && git branch --show-current 2>/dev/null || echo ''), \
#{?#{==:#{@pane_type},terminal},\
  ⚡ #{pane_current_command}, \
#{?#{==:#{@pane_type},logs},\
  📊 #(tail -1 #{pane_current_path}/cafedelic.log 2>/dev/null | cut -c1-30 || echo ''),}}}"

# Enable pane borders
set -g pane-border-status top
set -g pane-border-lines heavy
```

## Future Possibilities

1. **AI Status Indicators**: Show when Claude is thinking, typing, or waiting
2. **Collaboration Markers**: Indicate which agents are working on related files
3. **Progress Bars**: Show task completion using Unicode characters
4. **Mini Graphs**: Use Braille characters to show activity over time
5. **Emoji States**: Rich visual feedback with context-appropriate emojis

The combination of tmux's powerful customization and Cafedelic's agent context system creates endless possibilities for enhancing the multi-agent development experience!
