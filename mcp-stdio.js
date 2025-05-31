#!/usr/bin/env node
/**
 * Modern MCP stdio server for cafedelic tools
 * Uses McpServer class with modern .tool() syntax
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { z } from 'zod';
import { exec } from 'child_process';
import { promisify } from 'util';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const execAsync = promisify(exec);

// Path to scripts directory - relative to this module's location
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const SCRIPTS_DIR = join(__dirname, 'scripts');

// Create MCP server
const server = new McpServer({
  name: 'cafedelic-pane-tools',
  version: '0.1.0',
});

/**
 * Execute a shell script with error handling
 */
async function runScript(scriptPath, args = []) {
  try {
    const fullPath = join(SCRIPTS_DIR, scriptPath);
    const command = `"${fullPath}" ${args.map(arg => `"${arg}"`).join(' ')}`;
    const { stdout, stderr } = await execAsync(command);
    return { stdout: stdout.trim(), stderr: stderr.trim() };
  } catch (error) {
    throw new Error(error.stderr || error.message);
  }
}

// Pane Interaction Tools

server.tool(
  'send_keys_to_pane',
  {
    name: z.string().describe('The custom name of the pane'),
    text: z.string().describe('Keys/text to send to the pane')
  },
  async ({ name, text }) => {
    const result = await runScript('pane-management/send-keys-to-pane.sh', [name, text]);
    return {
      content: [{
        type: 'text',
        text: `Successfully sent keys/text to pane '${name}'`
      }]
    };
  }
);



server.tool(
  'send_special_key_to_pane',
  {
    name: z.string().describe('The custom name of the pane'),
    key: z.enum(['enter', 'escape', 'tab', 'ctrl-c', 'ctrl-d', 'ctrl-z', 
                 'up', 'down', 'left', 'right', 'home', 'end', 
                 'page-up', 'page-down']).describe('Special key to send')
  },
  async ({ name, key }) => {
    const result = await runScript('pane-management/send-special-key.sh', [name, key]);
    return {
      content: [{
        type: 'text',
        text: `Successfully sent '${key}' key to pane '${name}'`
      }]
    };
  }
);

server.tool(
  'send_ctrl_c_to_pane_by_name',
  {
    name: z.string().describe('The custom name of the pane'),
    double_tap: z.boolean().default(false).describe('Send C-c twice in quick succession (e.g., for exiting Claude Code)')
  },
  async ({ name, double_tap }) => {
    const result = await runScript('pane-management/send-ctrl-c.sh', [
      name, 
      double_tap ? 'true' : 'false'
    ]);
    return {
      content: [{
        type: 'text',
        text: `Successfully sent Ctrl-C ${double_tap ? '(double-tap) ' : ''}to pane '${name}'`
      }]
    };
  }
);

server.tool(
  'get_details_for_pane_by_name',
  {
    name: z.string().describe('The custom name of the pane')
  },
  async ({ name }) => {
    const result = await runScript('pane-management/get-pane-details.sh', [name]);
    return {
      content: [{
        type: 'text',
        text: `Pane details for '${name}':\n${result.stdout}`
      }]
    };
  }
);

// New Property-Based Tools
server.tool(
  'assign_pane_properties',
  {
    session: z.string().describe('The tmux session name'),
    window: z.union([z.string(), z.number()]).describe('Window name or index'),
    pane: z.number().describe('Pane index (0-based)'),
    name: z.string().optional().describe('Custom name for the pane (no spaces, colons, or dots)'),
    source: z.enum(['user', 'claude-desktop', 'claude-code', 'system']).optional().describe('Source context for the pane'),
    role: z.enum(['editor', 'terminal', 'logs', 'tests', 'debug', 'monitor']).optional().describe('Semantic role for the pane')
  },
  async ({ session, window, pane, name, source, role }) => {
    // Build arguments for the script
    const args = [session, window.toString(), pane.toString()];
    if (name) args.push('--name', name);
    if (source) args.push('--source', source);
    if (role) args.push('--role', role);
    
    const result = await runScript('pane-properties/assign-properties.sh', args);
    
    const properties = [];
    if (name) properties.push(`name='${name}'`);
    if (source) properties.push(`source='${source}'`);
    if (role) properties.push(`role='${role}'`);
    
    return {
      content: [{
        type: 'text',
        text: `Successfully assigned properties to pane ${session}:${window}.${pane}: ${properties.join(', ')}`
      }]
    };
  }
);

server.tool(
  'list_panes_by_properties',
  {
    source: z.enum(['user', 'claude-desktop', 'claude-code', 'system']).optional().describe('Filter by source'),
    role: z.enum(['editor', 'terminal', 'logs', 'tests', 'debug', 'monitor']).optional().describe('Filter by role'),
    name: z.string().optional().describe('Filter by name')
  },
  async ({ source, role, name }) => {
    const args = [];
    if (source) args.push('--source', source);
    if (role) args.push('--role', role);
    if (name) args.push('--name', name);
    
    const result = await runScript('pane-properties/list-panes-by-properties.sh', args);
    return {
      content: [{
        type: 'text',
        text: result.stdout || 'No panes found with specified properties'
      }]
    };
  }
);

server.tool(
  'find_pane_by_source_and_role',
  {
    source: z.enum(['user', 'claude-desktop', 'claude-code', 'system']).describe('Source to find'),
    role: z.enum(['editor', 'terminal', 'logs', 'tests', 'debug', 'monitor']).describe('Role to find')
  },
  async ({ source, role }) => {
    try {
      const result = await runScript('pane-properties/find-pane-by-source-and-role.sh', [source, role]);
      return {
        content: [{
          type: 'text',
          text: `Found pane with source='${source}' and role='${role}': ${result.stdout}`
        }]
      };
    } catch (error) {
      return {
        content: [{
          type: 'text',
          text: `No pane found with source='${source}' and role='${role}'`
        }]
      };
    }
  }
);

server.tool(
  'capture_pane_with_properties',
  {
    // Property-based selection
    source: z.enum(['user', 'claude-desktop', 'claude-code', 'system']).optional().describe('Source context for the pane'),
    role: z.enum(['editor', 'terminal', 'logs', 'tests', 'debug', 'monitor']).optional().describe('Semantic role for the pane'),
    name: z.string().optional().describe('Custom name of the pane'),
    
    // Capture range options
    start: z.union([z.number(), z.literal('-')]).optional().describe('Line number or "-" for beginning of history'),
    end: z.union([z.number(), z.literal('-')]).optional().describe('Line number or "-" for end'),
    last: z.number().optional().describe('Shortcut for last N lines (overrides start/end)'),
    
    // Output options
    join_lines: z.boolean().optional().describe('Join wrapped lines'),
    escape_sequences: z.boolean().optional().describe('Preserve colors/formatting'),
    preserve_trailing: z.boolean().optional().describe('Preserve trailing spaces'),
    
    // Search/filter
    grep: z.string().optional().describe('Pattern to search for in output'),
    grep_context: z.number().optional().default(0).describe('Lines of context around matches'),
    invert_match: z.boolean().optional().describe('Show non-matching lines')
  },
  async ({ source, role, name, start, end, last, join_lines, escape_sequences, preserve_trailing, grep, grep_context, invert_match }) => {
    // Build arguments for the script
    const args = [];
    
    // Property filters
    if (source) args.push('--source', source);
    if (role) args.push('--role', role);
    if (name) args.push('--name', name);
    
    // Capture range
    if (last !== undefined) {
      args.push('--last', last.toString());
    } else {
      if (start !== undefined) args.push('--start', start.toString());
      if (end !== undefined) args.push('--end', end.toString());
    }
    
    // Output options
    if (join_lines) args.push('--join-lines');
    if (escape_sequences) args.push('--escape-sequences');
    if (preserve_trailing) args.push('--preserve-trailing');
    
    // Search options
    if (grep) {
      args.push('--grep', grep);
      if (grep_context > 0) args.push('--grep-context', grep_context.toString());
      if (invert_match) args.push('--invert-match');
    }
    
    try {
      const result = await runScript('pane-properties/capture-pane-with-properties.sh', args);
      return {
        content: [{
          type: 'text',
          text: result.stdout
        }]
      };
    } catch (error) {
      return {
        content: [{
          type: 'text',
          text: `Failed to capture pane: ${error.message}`
        }]
      };
    }
  }
);

// Start the server
async function main() {
  try {
    const transport = new StdioServerTransport();
    await server.connect(transport);
    
    // Use stderr for startup message to avoid breaking MCP protocol
    process.stderr.write('Cafedelic MCP server started successfully\n');
  } catch (error) {
    process.stderr.write(`Failed to start Cafedelic MCP server: ${error.message}\n`);
    process.exit(1);
  }
}

main().catch((error) => {
  process.stderr.write(`Fatal error: ${error.message}\n`);
  process.exit(1);
});
