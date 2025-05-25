import path from 'path';
import { executeCommand, checkTmuxInstalled, tmuxSessionExists } from '../utils/shell.js';
import { logger } from '../utils/logger.js';

export async function launchIde(args: {
  session_name?: string;
  window_name?: string;
  start_processes?: boolean;
}) {
  const sessionName = args.session_name || 'cafedelic-main';
  const windowName = args.window_name || 'cafedelic';
  const startProcesses = args.start_processes !== false;
  
  try {
    // Check if tmux is installed
    if (!await checkTmuxInstalled()) {
      return {
        content: [{
          type: 'text',
          text: 'Error: tmux is not installed. Please install tmux first.'
        }]
      };
    }
    
    // Check if we're already in a tmux session
    const inTmux = process.env.TMUX !== undefined;
    
    if (!inTmux) {
      // If not in tmux, we need to launch a terminal with tmux
      return {
        content: [{
          type: 'text',
          text: 'Error: Not currently in a tmux session. Please run this command from within tmux.'
        }]
      };
    }
    
    // Get the path to our layout script
    const scriptPath = path.join(process.cwd(), 'scripts', 'create-ide-layout.sh');
    
    // Create the IDE layout in the current tmux session
    const layoutCommand = `bash "${scriptPath}" "${sessionName}" "${windowName}" "${startProcesses ? 'yes' : 'no'}"`;
    
    logger.info('Executing layout command', { command: layoutCommand });
    
    const result = await executeCommand(layoutCommand);
    
    if (result.code !== 0) {
      logger.error('Layout creation failed', { stderr: result.stderr });
      return {
        content: [{
          type: 'text',
          text: `Error creating IDE layout: ${result.stderr}`
        }]
      };
    }
    
    return {
      content: [{
        type: 'text',
        text: `IDE layout created successfully in session '${sessionName}', window '${windowName}'.\n\nNamed panes created:\n- cafedelic-activity\n- cafedelic-editor\n- cafedelic-terminal\n- cafedelic-logs\n\n${result.stdout}`
      }]
    };
    
  } catch (error: any) {
    logger.error('Launch IDE failed', { error: error.message });
    return {
      content: [{
        type: 'text',
        text: `Error launching IDE: ${error.message}`
      }]
    };
  }
}
