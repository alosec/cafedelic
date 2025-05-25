Add this project as an MCP server to local configuration:

1. Find the main entry point (look for index.js, dist/index.js, build/index.js, or check package.json "main" field)
2. Determine the correct command to run it (usually "node" or "npx")
3. Get the project name from package.json
4. Run: `claude mcp add-json --scope local "<name>" '{"command": "<cmd>", "args": ["<path-to-entry>"], "env": {}}'`