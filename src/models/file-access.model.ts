// File access tracking model
export interface FileAccess {
    id?: number;
    timestamp: Date;
    filePath: string;
    accessType: 'read' | 'write' | 'create' | 'delete';
    command: string;
    sessionId?: string;
    paneId?: string;
    contextSize?: number;
}

export interface FileAccessEvent {
    filePath: string;
    accessType: 'read' | 'write' | 'create' | 'delete';
    command: string;
    args?: any;
    shouldDisplay?: boolean;
}

export interface FileContext {
    path: string;
    size: number;
    addedAt: Date;
    lastAccessed: Date;
    accessCount: number;
}

export interface FileRelationship {
    id?: number;
    sourceFile: string;
    targetFile: string;
    relationshipType: 'imports' | 'references' | 'edits_together' | 'accessed_together';
    confidence: number;
    discoveredAt: Date;
    occurrenceCount: number;
}
