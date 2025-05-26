// MCP tool for viewing current output routing configuration
import { Tool } from '@modelcontextprotocol/sdk/types.js';
import { outputRouter } from '../services/output-router.service.js';
import { configManager } from '../config/cafedelic.config.js';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

export const getOutputRouting: Tool = {
  name: 'get_output_routing',
  description: 'Get current output routing configuration and pane assignments',
  inputSchema: {
    type: 'object',
    properties: {}
  }
};

export async function handleGetOutputRouting(): Promise<any> {
  const config = configManager.getConfig();
  const outputConfig = config.output || { defaultPane: '9:0.2', assignments: {} };
  
  // Get panes with cafedelic roles
  const panesWithRoles: any[] = [];
  
  try {
    // List all panes with their cafedelic_role
    const { stdout } = await execAsync(
      `tmux list-panes -a -F '#{pane_id} #{session_name}:#{window_index}.#{pane_index} #{@cafedelic_role}'`
    );
    
    const lines = stdout.trim().split('\n').filter(Boolean);
    for (const line of lines) {
      const parts = line.split(' ');
      if (parts.length >= 3 && parts[2]) {
        panesWithRoles.push({
          paneId: parts[0],
          target: parts[1],
          role: parts[2]
        });
      }
    }
  } catch (error) {
    // Ignore errors from tmux command
  }
  
  // Check which assignments are active
  const assignmentStatus: any = {};
  for (const [type, paneId] of Object.entries(outputConfig.assignments)) {
    const info = outputRouter.getDestinationInfo(type);
    assignmentStatus[type] = {
      destination: info.destination,
      source: info.source,
      exists: await checkPaneExists(info.destination)
    };
  }
  
  return {
    content: [{
      type: 'text',
      text: formatRoutingInfo({
        defaultPane: outputConfig.defaultPane,
        assignments: outputConfig.assignments,
        assignmentStatus,
        panesWithRoles,
        availableRoles: ['editor-output', 'activity-monitor', 'logs', 'terminal', 'files']
      })
    }]
  };
}

function formatRoutingInfo(info: any): string {
  let output = '📋 **Output Routing Configuration**\n\n';
  
  output += `**Default Pane:** ${info.defaultPane}\n\n`;
  
  output += '**Current Assignments:**\n';
  if (Object.keys(info.assignments).length > 0) {
    for (const [type, pane] of Object.entries(info.assignments)) {
      const status = info.assignmentStatus[type];
      const exists = status?.exists ? '✅' : '❌';
      output += `- ${type}: ${pane} ${exists}\n`;
    }
  } else {
    output += '- No assignments configured\n';
  }
  
  output += '\n**Panes with Cafedelic Roles:**\n';
  if (info.panesWithRoles.length > 0) {
    for (const pane of info.panesWithRoles) {
      output += `- ${pane.target}: ${pane.role}\n`;
    }
  } else {
    output += '- No panes have assigned roles\n';
  }
  
  output += '\n**Available Roles:** ' + info.availableRoles.join(', ');
  
  return output;
}

async function checkPaneExists(paneId: string): Promise<boolean> {
  try {
    await execAsync(`tmux list-panes -t "${paneId}" 2>/dev/null`);
    return true;
  } catch {
    return false;
  }
}
