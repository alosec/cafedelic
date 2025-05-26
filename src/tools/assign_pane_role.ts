// MCP tool for assigning cafedelic roles to tmux panes
import { Tool } from '@modelcontextprotocol/sdk/types.js';
import { outputRouter } from '../services/output-router.service.js';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

export const assignPaneRole: Tool = {
  name: 'assign_pane_role',
  description: 'Assign a cafedelic role to a tmux pane for output routing',
  inputSchema: {
    type: 'object',
    properties: {
      paneId: {
        type: 'string',
        description: 'Tmux pane ID (e.g., 9:0.2 or %1)'
      },
      role: {
        type: 'string',
        description: 'Role to assign (e.g., editor-output, activity-monitor, logs)',
        enum: ['editor-output', 'activity-monitor', 'logs', 'terminal', 'files']
      }
    },
    required: ['paneId', 'role']
  }
};

export async function handleAssignPaneRole(params: any): Promise<any> {
  const { paneId, role } = params;

  try {
    // Verify pane exists
    await execAsync(`tmux list-panes -t "${paneId}" 2>/dev/null`);
    
    // Assign the role
    const success = await outputRouter.assignPaneRole(paneId, role);
    
    if (success) {
      // Update configuration to use this pane for the role
      const { configManager } = await import('../config/cafedelic.config.js');
      const config = configManager.getConfig();
      
      // Update output assignments
      if (role === 'editor-output') {
        config.output.assignments['editor-output'] = paneId;
        configManager.updateConfig({ output: config.output });
      }
      
      return {
        content: [{
          type: 'text',
          text: `✅ Successfully assigned role '${role}' to pane ${paneId}\n\nThe pane ${paneId} will now receive all ${role} messages.`
        }]
      };
    } else {
      return {
        content: [{
          type: 'text',
          text: `❌ Failed to assign role '${role}' to pane ${paneId}`
        }]
      };
    }
  } catch (error) {
    const err = error as Error;
    return {
      content: [{
        type: 'text',
        text: `❌ Error: ${err.message}`
      }]
    };
  }
}
