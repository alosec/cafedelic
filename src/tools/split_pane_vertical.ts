// Tool to split the current pane vertically
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

export interface SplitPaneVerticalParams {
    target?: string;      // Target pane (name or ID)
    size?: number;        // Percentage or lines
    command?: string;     // Command to run in new pane
    name?: string;        // Name for new pane
}

export async function splitPaneVertical(params: SplitPaneVerticalParams = {}): Promise<string> {
    try {
        // Build tmux command - note: -v for vertical split
        let cmd = 'tmux split-window -v';
        
        if (params.target) {
            cmd += ` -t '${params.target}'`;
        }
        
        if (params.size) {
            cmd += ` -l ${params.size}`;
        }
        
        if (params.command) {
            cmd += ` '${params.command}'`;
        }
        
        // Execute split
        const { stdout } = await execAsync(cmd);
        const newPaneId = stdout.trim();
        
        // Assign name if provided
        if (params.name && newPaneId) {
            await execAsync(`tmux set-option -t '${newPaneId}' @pane_name '${params.name}'`);
        }
        
        return newPaneId || 'Pane split successfully';
    } catch (error: any) {
        throw new Error(`Failed to split pane: ${error.message}`);
    }
}

// MCP tool registration
export const split_pane_vertical = {
    description: 'Split the current pane vertically',
    parameters: {
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
    },
    handler: splitPaneVertical
};
