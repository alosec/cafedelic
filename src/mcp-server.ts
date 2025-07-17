/**
 * MCP Tools Server
 * Standalone server for MCP tool endpoints
 */

import express from 'express';
import { mcpRouter } from './mcp-tools/mcp-server.js';
import { getSessionService } from './services/session-service.js';
import { getProjectService } from './services/project-service.js';

const app = express();
const PORT = process.env.MCP_PORT || 3001;

// Middleware
app.use(express.json());

// MCP tools routes
app.use('/mcp', mcpRouter);

// Session API endpoints
const sessionService = getSessionService();
const projectService = getProjectService();

// GET /mcp/sessions - List sessions from database
app.get('/mcp/sessions', async (req, res) => {
  try {
    const { project, status, since, format = 'table' } = req.query;
    
    const result = await sessionService.listSessionsFormatted({
      projectId: project as string,
      status: status as string,
      since: since as string,
      format: format as 'table' | 'json' | 'csv'
    });
    
    if (format === 'json') {
      res.json(JSON.parse(result));
    } else {
      res.type('text/plain').send(result);
    }
  } catch (error) {
    res.status(500).json({ error: `Failed to list sessions: ${error}` });
  }
});

// GET /mcp/sessions/scan - Scan Claude Code files for sessions
app.get('/mcp/sessions/scan', async (req, res) => {
  try {
    const { path, format = 'table' } = req.query;
    
    const result = await sessionService.scanSessionsFormatted({
      path: path as string,
      format: format as 'table' | 'json' | 'csv'
    });
    
    if (format === 'json') {
      res.json(JSON.parse(result));
    } else {
      res.type('text/plain').send(result);
    }
  } catch (error) {
    res.status(500).json({ error: `Failed to scan sessions: ${error}` });
  }
});

// POST /mcp/sessions/load - Load discovered sessions into database
app.post('/mcp/sessions/load', async (req, res) => {
  try {
    const result = await sessionService.loadDiscoveredSessions();
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: `Failed to load sessions: ${error}` });
  }
});

// GET /mcp/projects - List projects from database
app.get('/mcp/projects', async (req, res) => {
  try {
    const { status, has_sessions, format = 'table' } = req.query;
    
    const result = await projectService.listProjectsFormatted({
      status: status as string,
      hasSessionsOnly: has_sessions === 'true',
      format: format as 'table' | 'json' | 'csv'
    });
    
    if (format === 'json') {
      res.json(JSON.parse(result));
    } else {
      res.type('text/plain').send(result);
    }
  } catch (error) {
    res.status(500).json({ error: `Failed to list projects: ${error}` });
  }
});

// GET /mcp/projects/scan - Scan filesystem for projects
app.get('/mcp/projects/scan', async (req, res) => {
  try {
    const { path, recursive, format = 'table' } = req.query;
    
    const result = await projectService.scanProjectsFormatted({
      path: path as string,
      recursive: recursive === 'true',
      format: format as 'table' | 'json' | 'csv'
    });
    
    if (format === 'json') {
      res.json(JSON.parse(result));
    } else {
      res.type('text/plain').send(result);
    }
  } catch (error) {
    res.status(500).json({ error: `Failed to scan projects: ${error}` });
  }
});

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', service: 'cafedelic-mcp-tools' });
});

// Start server
app.listen(PORT, () => {
  console.log(`MCP Tools server listening on port ${PORT}`);
  console.log(`Tools available at http://localhost:${PORT}/mcp/tools`);
});
