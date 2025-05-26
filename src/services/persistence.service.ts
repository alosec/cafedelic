// Persistence service for SQLite3 database operations
import * as sqlite3 from 'sqlite3';
import { open, Database } from 'sqlite';
import * as path from 'path';
import * as fs from 'fs/promises';
import { FileAccess, FileRelationship } from '../models/file-access.model';
import { Activity } from '../models/activity.model';
import { PaneState, Session } from '../models/pane-state.model';

export class PersistenceService {
    private db: Database | null = null;
    private dbPath: string;
    private schemaPath: string;

    constructor() {
        this.dbPath = path.join(process.cwd(), 'cafedelic.db');
        this.schemaPath = path.join(__dirname, '../database/schema.sql');
    }

    async initialize(): Promise<void> {
        try {
            // Open database connection
            this.db = await open({
                filename: this.dbPath,
                driver: sqlite3.Database
            });

            // Enable foreign keys
            await this.db.exec('PRAGMA foreign_keys = ON');

            // Initialize schema
            await this.initializeSchema();

            console.log('✅ Database initialized at:', this.dbPath);
        } catch (error) {
            console.error('❌ Failed to initialize database:', error);
            throw error;
        }
    }

    private async initializeSchema(): Promise<void> {
        if (!this.db) throw new Error('Database not initialized');

        try {
            const schema = await fs.readFile(this.schemaPath, 'utf-8');
            await this.db.exec(schema);
        } catch (error) {
            console.error('❌ Failed to initialize schema:', error);
            throw error;
        }
    }

    // File access operations
    async logFileAccess(access: FileAccess): Promise<void> {
        if (!this.db) throw new Error('Database not initialized');

        const sql = `
            INSERT INTO file_access (file_path, access_type, command, session_id, pane_id, context_size)
            VALUES (?, ?, ?, ?, ?, ?)
        `;

        await this.db.run(sql, [
            access.filePath,
            access.accessType,
            access.command,
            access.sessionId,
            access.paneId,
            access.contextSize
        ]);
    }

    async getRecentFileAccess(limit: number = 20): Promise<FileAccess[]> {
        if (!this.db) throw new Error('Database not initialized');

        const sql = `
            SELECT * FROM file_access
            ORDER BY timestamp DESC
            LIMIT ?
        `;

        return await this.db.all(sql, [limit]);
    }

    // Activity operations
    async logActivity(activity: Activity): Promise<void> {
        if (!this.db) throw new Error('Database not initialized');

        const sql = `
            INSERT INTO activities (raw_log, translated, command, args_json, session_id, context_window_size)
            VALUES (?, ?, ?, ?, ?, ?)
        `;

        await this.db.run(sql, [
            activity.rawLog,
            activity.translated,
            activity.command,
            activity.argsJson,
            activity.sessionId,
            activity.contextWindowSize
        ]);
    }

    // Pane state operations
    async updatePaneState(pane: PaneState): Promise<void> {
        if (!this.db) throw new Error('Database not initialized');

        const sql = `
            INSERT OR REPLACE INTO pane_states 
            (pane_name, pane_id, window_id, session_id, content_type, current_file, last_command, is_active)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        `;

        await this.db.run(sql, [
            pane.paneName,
            pane.paneId,
            pane.windowId,
            pane.sessionId,
            pane.contentType,
            pane.currentFile,
            pane.lastCommand,
            pane.isActive
        ]);
    }

    async getActivePanes(): Promise<PaneState[]> {
        if (!this.db) throw new Error('Database not initialized');

        const sql = `SELECT * FROM pane_states WHERE is_active = TRUE`;
        return await this.db.all(sql);
    }

    // Session operations
    async createSession(session: Session): Promise<void> {
        if (!this.db) throw new Error('Database not initialized');

        const sql = `
            INSERT INTO sessions (id, project_path, tmux_session)
            VALUES (?, ?, ?)
        `;

        await this.db.run(sql, [
            session.id,
            session.projectPath,
            session.tmuxSession
        ]);
    }

    async updateSessionStats(sessionId: string, filesAccessed: number, commandsRun: number): Promise<void> {
        if (!this.db) throw new Error('Database not initialized');

        const sql = `
            UPDATE sessions 
            SET total_files_accessed = ?, total_commands_run = ?
            WHERE id = ?
        `;

        await this.db.run(sql, [filesAccessed, commandsRun, sessionId]);
    }

    async close(): Promise<void> {
        if (this.db) {
            await this.db.close();
            this.db = null;
        }
    }
}

// Singleton instance
export const persistence = new PersistenceService();
