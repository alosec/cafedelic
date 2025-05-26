# Feature: Open directories in dired when Claude lists them

## Overview
When Claude uses the `ls` command to list a directory, automatically open that directory in dired within the Emacs frame. This would provide a seamless integration between Claude's file exploration and Emacs' powerful directory navigation.

## Expected Behavior
1. Claude executes `ls /path/to/directory`
2. Cafedelic detects the directory listing action
3. Automatically opens dired in the Emacs frame for `/path/to/directory`
4. User can immediately navigate and interact with files using dired

## Implementation Details
- Monitor Claude's tool usage for `ls` commands
- Extract the directory path from the command
- Use the existing Emacs service to open dired
- Ensure dired opens in the appropriate frame/window

## Benefits
- Seamless workflow between Claude's exploration and user's file management
- Leverages Emacs' powerful dired capabilities
- Reduces manual steps for users who want to interact with listed directories
- Maintains context between Claude's actions and user's workspace

## Technical Considerations
- Need to parse ls commands to extract directory paths
- Handle edge cases (relative paths, special directories)
- Ensure proper error handling if directory doesn't exist
- Consider configuration option to enable/disable this feature