#!/usr/bin/env node

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js';
import { launchIde } from './src/tools/launch-ide.js';
import { getContext } from './src/tools/get-context.js';
import { logger } from './src/utils/logger.js';

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
      name: 'launch_ide',
      description: 'Launch the IDE - combines terminal launch and tmux layout creation',
      inputSchema: {
        type: 'object',
        properties: {
          session_name: {
            type: 'string',
            description: 'Name for the tmux session (default: cafedelic-main)'
          },
          window_name: {
            type: 'string',
            description: 'Name for the tmux window (default: cafedelic)'
          },
          start_processes: {
            type: 'boolean',
            description: 'Whether to start processes in panes (default: true)'
          }
        }
      }
    },
    {
      name: 'get_context',
      description: 'Refresh knowledge and sync context with project intelligence',
      inputSchema: {
        type: 'object',
        properties: {
          project_path: {
            type: 'string',
            description: 'Path to project directory'
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
      case 'launch_ide':
        result = await launchIde(args || {});
        break;
      
      case 'get_context':
        result = await getContext(args || {});
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
