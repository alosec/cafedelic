// set-emacs-mode.tool.ts - Switch between emacs modes (daemon, pane-server, plain)

import { configManager } from '../config/cafedelic.config.js';
import { logger } from '../utils/logger.js';
import { paneEmacsManager } from '../services/pane-emacs-manager.service.js';

export const name = 'set_emacs_mode';

export const description = 'Set the emacs integration mode (daemon, pane-server, or plain)';

export const inputSchema = {
  type: 'object',
  properties: {
    mode: {
      type: 'string',
      enum: ['daemon', 'pane-server', 'plain'],
      description: 'The emacs mode to use'
    },
    defaultPane: {
      type: 'string',
      description: 'Default pane for pane-server mode (e.g., "9:0.2")',
      optional: true
    }
  },
  required: ['mode']
};

interface SetEmacsModeInput {
  mode: 'daemon' | 'pane-server' | 'plain';
  defaultPane?: string;
}

export async function handler(args: SetEmacsModeInput): Promise<any> {
  try {
    const config = configManager.getConfig();
    const previousMode = config.emacs.mode || 'daemon';
    
    // Update mode
    const updates: any = {
      emacs: {
        ...config.emacs,
        mode: args.mode
      }
    };
    
    // Update default pane if provided
    if (args.defaultPane && args.mode === 'pane-server') {
      updates.emacs.paneServers = {
        ...(config.emacs.paneServers || {
          defaultPane: '9:0.2',
          mapping: {},
          autoStart: true
        }),
        defaultPane: args.defaultPane
      };
    }
    
    // Save config
    configManager.updateConfig(updates);
    
    logger.info('Emacs mode changed', { 
      previousMode, 
      newMode: args.mode,
      defaultPane: args.defaultPane 
    });
    
    // Start health monitoring if switching to pane-server
    if (args.mode === 'pane-server') {
      paneEmacsManager.startHealthMonitoring();
    } else {
      paneEmacsManager.stopHealthMonitoring();
    }
    
    return {
      success: true,
      message: `Emacs mode changed from ${previousMode} to ${args.mode}`,
      currentConfig: {
        mode: args.mode,
        defaultPane: config.emacs.paneServers?.defaultPane
      }
    };
    
  } catch (error) {
    logger.error('Failed to set emacs mode', { error });
    return {
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error'
    };
  }
}
