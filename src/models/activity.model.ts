// Activity tracking model
export interface Activity {
    id?: number;
    timestamp: Date;
    rawLog: string;
    translated: string;
    command: string;
    argsJson?: string;
    sessionId?: string;
    contextWindowSize?: number;
}

export interface ActivityEvent {
    rawLog: string;
    translated: string;
    command: string;
    args?: any;
}

export interface ContextMetrics {
    totalFiles: number;
    totalSize: number;
    oldestFile?: string;
    mostAccessed: Array<{
        path: string;
        count: number;
    }>;
}

export interface ActivitySummary {
    recentActivities: string[];
    fileAccess: {
        recent: string[];
        frequent: string[];
    };
    commandPatterns: {
        [command: string]: number;
    };
}
