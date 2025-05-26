// Toggle Auto-Open MCP Tool
import { configManager } from '../config/cafedelic.config.js';
import { logger } from '../utils/logger.js';

export interface ToggleAutoOpenArgs {
  enable?: boolean; // If provided, sets to this value. If not, toggles current state
}

export async function toggleAutoOpen(args: ToggleAutoOpenArgs): Promise<any> {
  try {
    const config = configManager.getConfig();
    const currentState = config.emacs.autoOpen;
    
    let newState: boolean;
    if (args.enable !== undefined) {
      newState = args.enable;
      configManager.updateConfig({
        emacs: { ...config.emacs, autoOpen: newState }
      });
    } else {
      newState = configManager.toggleAutoOpen();
    }
    
    const action = args.enable !== undefined ? 'set' : 'toggled';
    const status = newState ? 'enabled' : 'disabled';
    
    logger.info(`Auto-open ${action} to ${status}`, { previousState: currentState, newState });
    
    return {
      content: [{
        type: 'text',
        text: `🔄 Auto-open ${status}
        
Previous state: ${currentState ? 'enabled' : 'disabled'}
New state: ${newState ? 'enabled' : 'disabled'}

${newState ? 
  '✅ Files accessed by Claude will now automatically open in Emacs editor' :
  '❌ Files will no longer automatically open (manual opening still available)'
}

Supported file types: ${config.emacs.supportedExtensions.join(', ')}`
      }]
    };
  } catch (error) {
    const err = error as Error;
    logger.error('Failed to toggle auto-open', { error: err.message });
    
    return {
      content: [{
        type: 'text',
        text: `❌ Failed to toggle auto-open: ${err.message}`
      }]
    };
  }
}
