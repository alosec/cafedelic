#!/usr/bin/env node

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js';
import { getActiveContext, setActivityStore } from './src/tools/get-active-context.js';
import { splitPaneHorizontal } from './src/tools/split_pane_horizontal.js';
import { splitPaneVertical } from './src/tools/split_pane_vertical.js';
import { toggleAutoOpen } from './src/tools/toggle_auto_open.js';
import { getEmacsStatus } from './src/tools/get_emacs_status.js';
import { assignPaneRole, handleAssignPaneRole } from './src/tools/assign_pane_role.js';
import { getOutputRouting, handleGetOutputRouting } from './src/tools/get_output_routing.js';
import { createTmexLayout } from './src/tools/create_tmex_layout.js';
import { captureLayoutState } from './src/tools/capture_layout_state.js';
import { clearTmuxPanes } from './src/tools/clear_tmux_panes.js';
import { logger } from './src/utils/logger.js';
import { WatcherService } from './src/services/watcher.service.js';
import { DesktopMCPWatcherService } from './src/services/desktop-mcp-watcher.service.js';
import { TranslatorService } from './src/services/translator.service.js';
import { ActivityStore } from './src/services/activity.store.js';
import { stateManager } from './src/services/state-manager.service.js';
import { emacsDaemonManager } from './src/services/emacs-daemon-manager.service.js';
import { configManager } from './src/config/cafedelic.config.js';

// Initialize services
const config = configManager.getConfig();
const watcher = config.desktopMCP.enabled ? new DesktopMCPWatcherService() : new WatcherService();
const translator = new TranslatorService();
const activityStore = new ActivityStore();

// Inject activity store into tools
setActivityStore(activityStore);

// Wire up services
watcher.on('log-entry', (entry) => {
  const activity = {
    raw: entry,
    translated: translator.translate(entry),
    timestamp: Date.now()
  };
  
  activityStore.add(activity);
  logger.info('Activity detected', { activity: activity.translated });
  
  // Emit to state manager
  stateManager.emit('dc:activity-logged', {
    rawLog: entry,
    translated: activity.translated,
    command: entry.command,
    args: entry.args
  });
  
  // Check for file access
  if (entry.command === 'read_file' && entry.args?.path) {
    stateManager.emit('dc:file-accessed', {
      filePath: entry.args.path,
      accessType: 'read',
      command: entry.command,
      args: entry.args,
      shouldDisplay: true
    });
  } else if (entry.command === 'write_file' && entry.args?.path) {
    stateManager.emit('dc:file-accessed', {
      filePath: entry.args.path,
      accessType: 'write',
      command: entry.command,
      args: entry.args,
      shouldDisplay: false
    });
  } else if (entry.command === 'list_directory' && entry.args?.path) {
    stateManager.emit('dc:directory-listed', {
      filePath: entry.args.path,
      accessType: 'list',
      command: entry.command,
      args: entry.args,
      shouldDisplay: true
    });
  }
});

// Initialize MCP server
const server = new Server({
  name: 'cafedelic',
  version: '0.1.0',
}, {
  capabilities: {
    tools: {}
  }
});

// Log server startup
logger.info('Initializing Cafedelic MCP server', { version: '0.1.0' });

// Register available tools
server.setRequestHandler(ListToolsRequestSchema, async () => ({
  tools: [
    {
      name: 'get_active_context',
      description: 'Get active development context including recent activity and files under review',
      inputSchema: {
        type: 'object',
        properties: {
          include_activity: {
            type: 'boolean',
            description: 'Include recent activity log (default: true)'
          },
          lookback_minutes: {
            type: 'number',
            description: 'How many minutes of activity to include (default: 5)'
          }
        }
      }
    },
    {
      name: 'split_pane_horizontal',
      description: 'Split the current pane horizontally',
      inputSchema: {
        type: 'object',
        properties: {
          target: {
            type: 'string',
            description: 'Target pane (name or ID)'
          },
          size: {
            type: 'number',
            description: 'Size in percentage or lines'
          },
          command: {
            type: 'string',
            description: 'Command to run in new pane'
          },
          name: {
            type: 'string',
            description: 'Name for the new pane'
          }
        }
      }
    },
    {
      name: 'split_pane_vertical',
      description: 'Split the current pane vertically',
      inputSchema: {
        type: 'object',
        properties: {
          target: {
            type: 'string',
            description: 'Target pane (name or ID)'
          },
          size: {
            type: 'number',
            description: 'Size in percentage or lines'
          },
          command: {
            type: 'string',
            description: 'Command to run in new pane'
          },
          name: {
            type: 'string',
            description: 'Name for the new pane'
          }
        }
      }
    },
    {
      name: 'toggle_auto_open',
      description: 'Toggle or set automatic file opening in Emacs when Claude accesses files',
      inputSchema: {
        type: 'object',
        properties: {
          enable: {
            type: 'boolean',
            description: 'Set to true/false to enable/disable, or omit to toggle current state'
          }
        }
      }
    },
    {
      name: 'get_emacs_status',
      description: 'Get current status of Emacs integration and configuration',
      inputSchema: {
        type: 'object',
        properties: {
          detailed: {
            type: 'boolean',
            description: 'Include detailed configuration information (default: false)'
          }
        }
      }
    },
    assignPaneRole,
    getOutputRouting,
    {
      name: 'create_tmex_layout',
      description: 'Create a tmex layout in a specified pane',
      inputSchema: {
        type: 'object',
        properties: {
          targetPane: {
            type: 'string',
            description: 'Target pane identifier (e.g., "9:0.1" or "%1")'
          },
          layout: {
            type: 'string',
            description: 'Tmex layout string (e.g., "111", "222", "12[34]5")'
          }
        },
        required: ['targetPane', 'layout']
      }
    },
    {
      name: 'capture_layout_state',
      description: 'Capture the current tmux layout state with geometric information',
      inputSchema: {
        type: 'object',
        properties: {
          target: {
            type: 'string',
            description: 'Target session:window (default: current window)'
          }
        }
      }
    },
    {
      name: 'clear_tmux_panes',
      description: 'Clear or reset tmux panes with various strategies',
      inputSchema: {
        type: 'object',
        properties: {
          target: {
            type: 'string',
            description: 'Target pane/session identifier (e.g., "%1", "9:0.1", "mysession")'
          },
          mode: {
            type: 'string',
            enum: ['pane', 'session', 'reset-layout', 'reset-keep-two'],
            description: 'Clear mode: pane (clear single pane), session (kill session), reset-layout (single pane), reset-keep-two (keep panes 0 and 1)'
          },
          verify: {
            type: 'boolean',
            description: 'Capture before/after states to verify operation (default: false)'
          }
        },
        required: ['target', 'mode']
      }
    }
  ]
}));

// Handle tool calls
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: rawArgs } = request.params;
  
  // Handle nested args structure from MCP client
  let args = rawArgs;
  if (rawArgs && rawArgs.args && typeof rawArgs.args === 'string') {
    try {
      args = JSON.parse(rawArgs.args);
    } catch (e) {
      logger.error('Failed to parse nested args', { error: (e as Error).message, rawArgs });
    }
  }

  logger.info('Tool called', { tool: name, args });

  try {
    let result;
    
    switch (name) {
      case 'get_active_context':
        result = await getActiveContext(args || {});
        break;
      
      case 'split_pane_horizontal':
        result = {
          content: [{
            type: 'text',
            text: await splitPaneHorizontal(args || {})
          }]
        };
        break;
      
      case 'split_pane_vertical':
        result = {
          content: [{
            type: 'text',
            text: await splitPaneVertical(args || {})
          }]
        };
        break;
      
      case 'toggle_auto_open':
        result = await toggleAutoOpen(args || {});
        break;
      
      case 'get_emacs_status':
        result = await getEmacsStatus(args || {});
        break;
      
      case 'assign_pane_role':
        result = await handleAssignPaneRole(args || {});
        break;
      
      case 'get_output_routing':
        result = await handleGetOutputRouting();
        break;
      
      case 'create_tmex_layout':
        result = {
          content: [{
            type: 'text',
            text: await createTmexLayout(args as any || {})
          }]
        };
        break;
      
      case 'capture_layout_state':
        result = {
          content: [{
            type: 'text',
            text: JSON.stringify(await captureLayoutState(args || {}), null, 2)
          }]
        };
        break;
      
      case 'clear_tmux_panes':
        result = {
          content: [{
            type: 'text',
            text: JSON.stringify(await clearTmuxPanes(args as any || {}), null, 2)
          }]
        };
        break;
      
      default:
        logger.error('Unknown tool requested', { tool: name });
        result = {
          content: [{
            type: 'text',
            text: `Unknown tool: ${name}`
          }]
        };
    }
    
    logger.info('Tool executed successfully', { tool: name });
    return result;
  } catch (error) {
    const err = error as Error;
    logger.error('Tool execution failed', { tool: name, error: err.message, stack: err.stack });
    return {
      content: [{
        type: 'text',
        text: `Error: ${err.message}`
      }]
    };
  }
});

// Start the server
async function main() {
  try {
    await logger.info('Starting Cafedelic MCP server...');
    
    // Initialize state manager
    const projectPath = process.cwd();
    await stateManager.initialize(projectPath);
    await logger.info('State manager initialized');
    
    // Start the appropriate log watcher
    const watcherType = config.desktopMCP.enabled ? 'Desktop MCP' : 'DC';
    await watcher.start();
    await logger.info(`${watcherType} log watcher started`);
    
    // If Desktop MCP fails and fallback is enabled, try DC logs
    if (config.desktopMCP.enabled && config.desktopMCP.fallbackToDC) {
      watcher.on('error', async (error) => {
        logger.warn('Desktop MCP watcher failed, falling back to DC logs', { error });
        const dcWatcher = new WatcherService();
        await dcWatcher.start();
        // Transfer event listeners
        watcher.removeAllListeners('log-entry');
        dcWatcher.on('log-entry', (entry) => watcher.emit('log-entry', entry));
      });
    }
    
    // Note: Emacs daemon manager uses lazy initialization
    // It will start automatically on first file operation
    await logger.info('Emacs daemon manager ready (lazy init)');
    
    const transport = new StdioServerTransport();
    await server.connect(transport);
    
    await logger.info('Cafedelic MCP server started successfully');
    // Use stderr for startup message to avoid breaking MCP protocol
    process.stderr.write('Cafedelic MCP server started\n');
  } catch (error) {
    const err = error as Error;
    await logger.error('Failed to start Cafedelic MCP server', { error: err.message, stack: err.stack });
    throw error;
  }
}

main().catch(async (error) => {
  const err = error as Error;
  await logger.error('Fatal error', { error: err.message, stack: err.stack });
  // Use stderr for error output to avoid breaking MCP protocol
  process.stderr.write(`Fatal error: ${err.message}\n${err.stack}\n`);
  process.exit(1);
});

// Graceful shutdown
process.on('SIGINT', async () => {
  logger.info('Shutting down Cafedelic...');
  await emacsDaemonManager.shutdown();
  process.exit(0);
});

process.on('SIGTERM', async () => {
  logger.info('Shutting down Cafedelic...');
  await emacsDaemonManager.shutdown();
  process.exit(0);
});
