#!/usr/bin/env node
/**
 * MCP stdio wrapper for cafedelic tools
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { toolDefinitions } from './src/mcp-tools/mcp-server.js';
import { paneNamingTools, paneInteractionTools, routingTools } from './src/mcp-tools/pane-tools.js';

const server = new Server({
  name: 'cafedelic-pane-tools',
  version: '0.1.0',
}, {
  capabilities: {
    tools: {}
  }
});

// Register tool handlers
server.setRequestHandler('tools/list', async () => ({
  tools: toolDefinitions
}));

server.setRequestHandler('tools/call', async (request) => {
  const { name, arguments: args } = request.params;
  
  // Route to appropriate handler
  switch(name) {
    case 'assign_name_to_pane':
      return await paneNamingTools.assignNameToPane(args.session, args.window, args.pane, args.name);
    // ... add other handlers
  }
});

const transport = new StdioServerTransport();
server.connect(transport);
