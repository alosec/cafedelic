/**
 * MCP Server for Cafedelic Pane Management
 * Exposes pane management tools via MCP protocol
 */

import express from 'express';
import { paneNamingTools, paneInteractionTools } from './pane-tools.js';

const router = express.Router();

// Tool definitions for MCP
const toolDefinitions = [
  {
    name: 'assign_name_to_pane',
    description: 'Assign a custom name to a tmux pane for easy reference',
    inputSchema: {
      type: 'object',
      properties: {
        session: { type: 'string', description: 'The tmux session name' },
        window: { type: ['string', 'number'], description: 'Window name or index' },
        pane: { type: 'number', description: 'Pane index (0-based)' },
        name: { type: 'string', description: 'Custom name for the pane (no spaces, colons, or dots)' }
      },
      required: ['session', 'window', 'pane', 'name']
    }
  },
  {
    name: 'read_pane_by_name',
    description: 'Read the last N lines from a named pane',
    inputSchema: {
      type: 'object',
      properties: {
        name: { type: 'string', description: 'The custom name of the pane' },
        lines: { type: 'number', description: 'Number of lines to read (default: 100)', default: 100 }
      },
      required: ['name']
    }
  },
  {
    name: 'send_keys_to_pane',
    description: 'Send keys/text to a named pane',
    inputSchema: {
      type: 'object',
      properties: {
        name: { type: 'string', description: 'The custom name of the pane' },
        text: { type: 'string', description: 'Keys/text to send to the pane' }
      },
      required: ['name', 'text']
    }
  },
  {
    name: 'list_named_panes',
    description: 'List all panes that have been assigned custom names'
  },
  {
    name: 'send_special_key_to_pane',
    description: 'Send a special key sequence to a named pane',
    inputSchema: {
      type: 'object',
      properties: {
        name: { type: 'string', description: 'The custom name of the pane' },
        key: {
          type: 'string',
          enum: ['enter', 'escape', 'tab', 'ctrl-c', 'ctrl-d', 'ctrl-z', 
                 'up', 'down', 'left', 'right', 'home', 'end', 
                 'page-up', 'page-down'],
          description: 'Special key to send'
        }
      },
      required: ['name', 'key']
    }
  },
  {
    name: 'send_ctrl_c_to_pane',
    description: 'Send C-c to a named pane with optional double-tap for applications like Claude Code',
    inputSchema: {
      type: 'object',
      properties: {
        name: { type: 'string', description: 'The custom name of the pane' },
        double_tap: {
          type: 'boolean',
          description: 'Send C-c twice in quick succession (e.g., for exiting Claude Code)',
          default: false
        }
      },
      required: ['name']
    }
  }
];

// Tool handlers
const toolHandlers: Record<string, Function> = {
  assign_name_to_pane: async (params: any) => {
    return await paneNamingTools.assignNameToPane(
      params.session,
      params.window,
      params.pane,
      params.name
    );
  },
  
  read_pane_by_name: async (params: any) => {
    return await paneNamingTools.readPaneByName(params.name, params.lines);
  },
  
  send_keys_to_pane: async (params: any) => {
    return await paneNamingTools.sendKeysToPane(params.name, params.text);
  },
  
  list_named_panes: async () => {
    return await paneNamingTools.listNamedPanes();
  },
  
  send_special_key_to_pane: async (params: any) => {
    return await paneInteractionTools.sendSpecialKeyToPane(params.name, params.key);
  },
  
  send_ctrl_c_to_pane: async (params: any) => {
    return await paneInteractionTools.sendCtrlCToPane(params.name, params.double_tap);
  }
};

// Express routes for MCP
router.post('/tools', (req: any, res: any) => {
  res.json({ tools: toolDefinitions });
});

router.post('/call-tool', async (req: any, res: any) => {
  const { name, arguments: args } = req.body;
  
  if (!toolHandlers[name]) {
    return res.status(404).json({ error: `Tool ${name} not found` });
  }
  
  try {
    const result = await toolHandlers[name](args);
    res.json({ content: [{ type: 'text', text: JSON.stringify(result, null, 2) }] });
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

export { router as mcpRouter, toolDefinitions };
