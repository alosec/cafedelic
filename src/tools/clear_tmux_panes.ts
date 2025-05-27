// Tool to clear tmux panes with various strategies
import { exec } from 'child_process';
import { promisify } from 'util';
import { logger } from '../utils/logger.js';
import { captureLayoutState, LayoutState } from './capture_layout_state.js';

const execAsync = promisify(exec);

export interface ClearTmuxPanesParams {
    target: string;
    mode: 'pane' | 'session' | 'reset-layout' | 'reset-keep-two';
    verify?: boolean;
}

export interface ClearResult {
    success: boolean;
    beforeState?: LayoutState;
    afterState?: LayoutState;
    message: string;
    errors?: string[];
}

export async function clearTmuxPanes(params: ClearTmuxPanesParams): Promise<ClearResult> {
    const { target, mode, verify = false } = params;
    
    if (!target || !mode) {
        throw new Error('Both target and mode are required');
    }
    
    const result: ClearResult = {
        success: false,
        message: '',
        errors: []
    };
    
    try {
        logger.info('Clearing tmux panes', { target, mode, verify });
        
        // Capture before state if verify is enabled
        if (verify && (mode === 'reset-layout' || mode === 'reset-keep-two')) {
            try {
                const sessionWindow = mode === 'pane' ? target.split('.')[0] : target;
                result.beforeState = await captureLayoutState({ target: sessionWindow });
            } catch (e) {
                logger.warn('Could not capture before state', { error: (e as Error).message });
            }
        }
        
        // Use the tmux-clear.sh script
        const scriptPath = '/home/alex/code/cafedelic/scripts/tmux-clear.sh';
        const cmd = `${scriptPath} "${target}" "${mode}"`;
        
        const { stdout, stderr } = await execAsync(cmd);
        
        if (stderr) {
            result.errors?.push(stderr);
            logger.warn('Clear operation had warnings', { stderr });
        }
        
        result.message = stdout.trim();
        result.success = !stderr || stderr.includes('not found');
        
        // Capture after state if verify is enabled
        if (verify && (mode === 'reset-layout' || mode === 'reset-keep-two')) {
            // Wait a bit for tmux to settle
            await new Promise(resolve => setTimeout(resolve, 100));
            
            try {
                const sessionWindow = mode === 'pane' ? target.split('.')[0] : target;
                result.afterState = await captureLayoutState({ target: sessionWindow });
                
                // Verify the operation worked as expected
                if (mode === 'reset-layout') {
                    result.success = result.afterState.totalPanes === 1;
                    if (!result.success) {
                        result.errors?.push(`Expected 1 pane after reset, but found ${result.afterState.totalPanes}`);
                    }
                } else if (mode === 'reset-keep-two') {
                    result.success = result.afterState.totalPanes <= 2;
                    if (!result.success) {
                        result.errors?.push(`Expected <= 2 panes after reset, but found ${result.afterState.totalPanes}`);
                    }
                }
            } catch (e) {
                logger.warn('Could not capture after state', { error: (e as Error).message });
            }
        }
        
        logger.info('Clear operation completed', { 
            success: result.success, 
            message: result.message 
        });
        
        return result;
    } catch (error: any) {
        logger.error('Failed to clear tmux panes', { error: error.message });
        result.errors?.push(error.message);
        result.message = `Failed to clear tmux panes: ${error.message}`;
        return result;
    }
}

// MCP tool registration
export const clear_tmux_panes = {
    description: 'Clear or reset tmux panes with various strategies',
    parameters: {
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
    },
    handler: clearTmuxPanes
};
