// Tool to create tmex layouts in specified panes
import { exec } from 'child_process';
import { promisify } from 'util';
import { logger } from '../utils/logger.js';

const execAsync = promisify(exec);

export interface CreateTmexLayoutParams {
    targetPane: string;   // e.g., "9:0.1" or "%1"
    layout: string;       // e.g., "111", "12[34]5"
}

export async function createTmexLayout(params: CreateTmexLayoutParams): Promise<string> {
    const { targetPane, layout } = params;
    
    if (!targetPane || !layout) {
        throw new Error('Both targetPane and layout are required');
    }
    
    try {
        logger.info('Creating tmex layout', { targetPane, layout });
        
        // Use the send-tmex.sh script
        const scriptPath = '/app/scripts/send-tmex.sh';
        const cmd = `${scriptPath} "${layout}" "${targetPane}"`;
        
        const { stdout, stderr } = await execAsync(cmd);
        
        if (stderr && !stderr.includes('Sending')) {
            logger.warn('Tmex layout creation had warnings', { stderr });
        }
        
        const result = stdout || `Layout "${layout}" deployed to pane ${targetPane}`;
        logger.info('Tmex layout created successfully', { result });
        
        return result.trim();
    } catch (error: any) {
        logger.error('Failed to create tmex layout', { error: error.message });
        throw new Error(`Failed to create tmex layout: ${error.message}`);
    }
}

// MCP tool registration
export const create_tmex_layout = {
    description: 'Create a tmex layout in a specified pane',
    parameters: {
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
    },
    handler: createTmexLayout
};
