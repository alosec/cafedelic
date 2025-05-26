#!/usr/bin/env node

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js';
import { getActiveContext, setActivityStore } from './src/tools/get-active-context.js';
import { splitPaneHorizontal } from './src/tools/split_pane_horizontal.js';
import { splitPaneVertical } from './src/tools/split_pane_vertical.js';
import { logger } from './src/utils/logger.js';
import { WatcherService } from './src/services/watcher.service.js';
import { TranslatorService } from './src/services/translator.service.js';
import { ActivityStore } from './src/services/activity.store.js';
import { stateManager } from './src/services/state-manager.service.js';

// Initialize services
const watcher = new WatcherService();
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
    
    // Start the DC log watcher
    await watcher.start();
    await logger.info('DC log watcher started');
    
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
