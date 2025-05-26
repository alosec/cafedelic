// Central state management engine
import { EventEmitter } from 'events';
import { v4 as uuidv4 } from 'uuid';
import { PersistenceService, persistence } from './persistence.service.js';
import { emacsService } from './emacs.service.js';
import { FileAccessEvent } from '../models/file-access.model.js';
import { ActivityEvent } from '../models/activity.model.js';
import { PaneState, Session } from '../models/pane-state.model.js';

export interface StateChangeEvent {
    type: 'file-accessed' | 'activity-logged' | 'pane-updated' | 'session-started';
    data: any;
    timestamp: Date;
}

export class StateManager extends EventEmitter {
    private currentSessionId: string | null = null;
    private persistence: PersistenceService;
    private initialized: boolean = false;

    constructor() {
        super();
        this.persistence = persistence;
    }

    async initialize(projectPath: string): Promise<void> {
        if (this.initialized) return;

        try {
            // Initialize database
            await this.persistence.initialize();

            // Create new session
            this.currentSessionId = uuidv4();
            const session: Session = {
                id: this.currentSessionId,
                startedAt: new Date(),
                projectPath,
                totalFilesAccessed: 0,
                totalCommandsRun: 0
            };

            await this.persistence.createSession(session);
            
            // Setup event handlers
            this.setupEventHandlers();
            
            this.initialized = true;
            console.error('✅ State manager initialized with session:', this.currentSessionId);
        } catch (error) {
            console.error('❌ Failed to initialize state manager:', error);
            throw error;
        }
    }

    private setupEventHandlers(): void {
        // Wire up internal event handling
        this.on('dc:file-accessed', this.handleFileAccess.bind(this));
        this.on('dc:directory-listed', this.handleDirectoryListing.bind(this));
        this.on('dc:command-executed', this.handleCommand.bind(this));
        this.on('dc:activity-logged', this.handleActivity.bind(this));
    }

    // Handle file access events from DC logs
    async handleFileAccess(event: FileAccessEvent): Promise<void> {
        if (!this.currentSessionId) return;

        try {
            await this.persistence.logFileAccess({
                filePath: event.filePath,
                accessType: event.accessType,
                command: event.command,
                sessionId: this.currentSessionId,
                timestamp: new Date()
            });

            // Emit state change
            this.emit('state:changed', {
                type: 'file-accessed',
                data: event,
                timestamp: new Date()
            } as StateChangeEvent);

            console.error(`📁 File accessed: ${event.filePath} (${event.accessType}`);

            // Trigger automatic file opening in emacs for read operations
            if (event.accessType === 'read' && event.shouldDisplay) {
                this.openFileInEmacs(event.filePath);
            }
            // Trigger automatic directory opening in dired for list operations
            else if (event.accessType === 'list' && event.shouldDisplay) {
                this.openDirectoryInEmacs(event.filePath);
            }
        } catch (error) {
            console.error('Failed to log file access:', error);
        }
    }

    // Handle directory listing events from DC logs
    async handleDirectoryListing(event: FileAccessEvent): Promise<void> {
        if (!this.currentSessionId) return;
        
        // Forward to handleFileAccess with list type
        await this.handleFileAccess({
            ...event,
            accessType: 'list',
            shouldDisplay: true
        });
    }

    // Open file in emacs (async, non-blocking)
    private async openFileInEmacs(filePath: string): Promise<void> {
        try {
            const result = await emacsService.openFile(filePath);
            if (result.success) {
                console.error(`📂 Auto-opened in emacs: ${filePath} (${result.bufferCount} total buffers`);
            } else {
                console.error(`⚠️  Auto-open skipped: ${result.message} - ${filePath}`);
            }
        } catch (error) {
            console.error(`❌ Failed to auto-open file: ${filePath}`, error);
        }
    }

    // Open directory in emacs dired (async, non-blocking)
    private async openDirectoryInEmacs(directoryPath: string): Promise<void> {
        try {
            const result = await emacsService.openDirectory(directoryPath);
            if (result.success) {
                console.error(`📁 Auto-opened directory in dired: ${directoryPath}`);
            } else {
                console.error(`⚠️  Auto-open directory skipped: ${result.message} - ${directoryPath}`);
            }
        } catch (error) {
            console.error(`❌ Failed to auto-open directory: ${directoryPath}`, error);
        }
    }

    // Handle command execution events
    async handleCommand(event: any): Promise<void> {
        // Future implementation
        console.error(`⚡ Command executed:`, event);
    }

    // Handle activity logging
    async handleActivity(event: ActivityEvent): Promise<void> {
        if (!this.currentSessionId) return;

        try {
            await this.persistence.logActivity({
                rawLog: event.rawLog,
                translated: event.translated,
                command: event.command,
                argsJson: event.args ? JSON.stringify(event.args) : undefined,
                sessionId: this.currentSessionId,
                timestamp: new Date()
            });

            this.emit('state:changed', {
                type: 'activity-logged',
                data: event,
                timestamp: new Date()
            } as StateChangeEvent);
        } catch (error) {
            console.error('Failed to log activity:', error);
        }
    }

    // Get current state summary
    async getStateSummary(): Promise<any> {
        const recentFiles = await this.persistence.getRecentFileAccess(10);
        const activePanes = await this.persistence.getActivePanes();

        return {
            sessionId: this.currentSessionId,
            recentFiles,
            activePanes,
            timestamp: new Date()
        };
    }

    getCurrentSessionId(): string | null {
        return this.currentSessionId;
    }

    async shutdown(): Promise<void> {
        await this.persistence.close();
        this.removeAllListeners();
        this.initialized = false;
    }
}

// Singleton instance
export const stateManager = new StateManager();
