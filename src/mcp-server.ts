/**
 * MCP Tools Server
 * Standalone server for MCP tool endpoints
 */

import express from 'express';
import { mcpRouter } from './mcp-tools/mcp-server.js';

const app = express();
const PORT = process.env.MCP_PORT || 3001;

// Middleware
app.use(express.json());

// MCP tools routes
app.use('/mcp', mcpRouter);

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', service: 'cafedelic-mcp-tools' });
});

// Start server
app.listen(PORT, () => {
  console.log(`MCP Tools server listening on port ${PORT}`);
  console.log(`Tools available at http://localhost:${PORT}/mcp/tools`);
});
