// Pane state tracking model
export interface PaneState {
    id?: number;
    timestamp?: Date;
    paneName: string;
    paneId: string;
    windowId: string;
    sessionId: string;
    contentType: 'editor' | 'terminal' | 'monitor' | 'tree' | 'logs';
    currentFile?: string;
    lastCommand?: string;
    isActive: boolean;
}

export interface PaneConfig {
    name: string;
    type: 'editor' | 'terminal' | 'monitor' | 'tree' | 'logs';
    size?: number;
    command?: string;
    target?: string;
}

export interface FullPaneState extends PaneState {
    size: {
        width: number;
        height: number;
    };
    position: {
        x: number;
        y: number;
    };
    content?: string[];
    focused: boolean;
}

export interface WindowState {
    id: string;
    name: string;
    layout: string;
    panes: PaneState[];
    active: boolean;
}

export interface Session {
    id: string;
    startedAt: Date;
    endedAt?: Date;
    projectPath: string;
    tmuxSession?: string;
    totalFilesAccessed: number;
    totalCommandsRun: number;
}
