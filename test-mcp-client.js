#!/usr/bin/env node
/**
 * Test the MCP server with a simple tool call
 */

import { Client } from '@modelcontextprotocol/sdk/client/index.js';
import { StdioClientTransport } from '@modelcontextprotocol/sdk/client/stdio.js';

async function testMcpServer() {
  try {
    console.log('Starting MCP client test...');
    
    const transport = new StdioClientTransport({
      command: 'node',
      args: ['mcp-stdio.js']
    });
    
    const client = new Client({
      name: 'test-client',
      version: '1.0.0'
    });
    
    console.log('Connecting to MCP server...');
    await client.connect(transport);
    
    console.log('Listing available tools...');
    const tools = await client.listTools();
    console.log(`Found ${tools.tools.length} tools:`);
    tools.tools.forEach(tool => {
      console.log(`  - ${tool.name}: ${tool.description}`);
    });
    
    console.log('\nTesting list_named_panes tool...');
    const result = await client.callTool({
      name: 'list_named_panes',
      arguments: {}
    });
    
    console.log('Tool result:', result);
    
    await client.close();
    console.log('Test completed successfully!');
    
  } catch (error) {
    console.error('Test failed:', error.message);
  }
}

testMcpServer();
