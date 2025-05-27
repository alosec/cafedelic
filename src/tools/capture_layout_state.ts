// Tool to capture tmux layout state
import { exec } from 'child_process';
import { promisify } from 'util';
import { logger } from '../utils/logger.js';

const execAsync = promisify(exec);

export interface CaptureLayoutStateParams {
    target?: string;      // session:window (default: current)
}

export interface LayoutPane {
    id: number;
    position: { x: number; y: number };
    size: { width: number; height: number };
    boundaries?: { x: [number, number]; y: [number, number] };
    command: string;
}

export interface LayoutState {
    timestamp: string;
    sessionWindow: string;
    windowSize: { width: number; height: number };
    layoutString: string;
    panes: LayoutPane[];
    totalPanes: number;
}

export async function captureLayoutState(params: CaptureLayoutStateParams = {}): Promise<LayoutState> {
    const { target = '.' } = params;
    
    try {
        logger.info('Capturing tmux layout state', { target });
        
        // Use the capture-tmux-layout.sh script
        const scriptPath = '/home/alex/code/cafedelic/scripts/capture-tmux-layout.sh';
        const cmd = `${scriptPath} "${target}" console`;
        
        const { stdout } = await execAsync(cmd);
        
        // Parse the output
        const lines = stdout.split('\n');
        const state: Partial<LayoutState> = {
            timestamp: new Date().toISOString(),
            panes: []
        };
        
        // Parse the structured output
        let inPanesSection = false;
        for (const line of lines) {
            if (line.includes('Session:') && line.includes('Window:')) {
                const match = line.match(/Session: (\S+) \| Window: (\S+)/);
                if (match) {
                    state.sessionWindow = `${match[1]}:${match[2]}`;
                }
            } else if (line.includes('Window Size:')) {
                const match = line.match(/Window Size: (\d+)x(\d+)/);
                if (match) {
                    state.windowSize = {
                        width: parseInt(match[1]),
                        height: parseInt(match[2])
                    };
                }
            } else if (line.includes('Layout String:')) {
                // Next non-empty line is the layout string
                const idx = lines.indexOf(line);
                for (let i = idx + 1; i < lines.length; i++) {
                    if (lines[i].trim()) {
                        state.layoutString = lines[i].trim();
                        break;
                    }
                }
            } else if (line.includes('ID | Position')) {
                inPanesSection = true;
                continue;
            } else if (line.includes('Total Panes:')) {
                const match = line.match(/Total Panes: (\d+)/);
                if (match) {
                    state.totalPanes = parseInt(match[1]);
                }
                inPanesSection = false;
            } else if (inPanesSection && line.includes('|')) {
                // Parse pane line: "0  | (0,0)          | 176×23      | emacs"
                const parts = line.split('|').map(p => p.trim());
                if (parts.length >= 4 && !parts[0].includes('--')) {
                    const id = parseInt(parts[0]);
                    const posMatch = parts[1].match(/\((\d+),(\d+)\)/);
                    const sizeMatch = parts[2].match(/(\d+)×(\d+)/);
                    
                    if (posMatch && sizeMatch) {
                        state.panes!.push({
                            id,
                            position: {
                                x: parseInt(posMatch[1]),
                                y: parseInt(posMatch[2])
                            },
                            size: {
                                width: parseInt(sizeMatch[1]),
                                height: parseInt(sizeMatch[2])
                            },
                            command: parts[3] || 'unknown'
                        });
                    }
                }
            } else if (line.includes('Pane') && line.includes(': x[')) {
                // Parse boundary line: "Pane 0: x[0-176] y[0-23]"
                const match = line.match(/Pane (\d+): x\[(\d+)-(\d+)\] y\[(\d+)-(\d+)\]/);
                if (match) {
                    const paneId = parseInt(match[1]);
                    const pane = state.panes!.find(p => p.id === paneId);
                    if (pane) {
                        pane.boundaries = {
                            x: [parseInt(match[2]), parseInt(match[3])],
                            y: [parseInt(match[4]), parseInt(match[5])]
                        };
                    }
                }
            }
        }
        
        logger.info('Layout state captured successfully', { 
            sessionWindow: state.sessionWindow,
            totalPanes: state.totalPanes 
        });
        
        return state as LayoutState;
    } catch (error: any) {
        logger.error('Failed to capture layout state', { error: error.message });
        throw new Error(`Failed to capture layout state: ${error.message}`);
    }
}

// MCP tool registration
export const capture_layout_state = {
    description: 'Capture the current tmux layout state with geometric information',
    parameters: {
        type: 'object',
        properties: {
            target: {
                type: 'string',
                description: 'Target session:window (default: current window)'
            }
        }
    },
    handler: captureLayoutState
};
