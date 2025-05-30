#!/bin/bash
# test-property-system.sh - Test the new property-based pane system

echo "Testing Multi-Dimensional Pane Property System"
echo "============================================="
echo ""

# Test 1: Assign properties to a test pane
echo "Test 1: Assigning properties to pane dev:0.0"
./scripts/pane-properties/assign-properties.sh dev 0 0 \
  --name "test-editor" \
  --source "claude-desktop" \
  --role "editor"

echo ""

# Test 2: List panes by properties
echo "Test 2: Listing all panes with properties"
./scripts/pane-properties/list-panes-by-properties.sh

echo ""

# Test 3: Find pane by source and role
echo "Test 3: Finding pane with source='claude-desktop' and role='editor'"
./scripts/pane-properties/find-pane-by-source-and-role.sh claude-desktop editor || echo "Not found"

echo ""

# Test 4: Find best pane for role with fallback
echo "Test 4: Finding best pane for role='editor'"
./scripts/pane-properties/find-best-pane-for-role.sh editor claude-desktop || echo "Not found"

echo ""

# Test 5: Filter panes by properties
echo "Test 5: Listing only claude-desktop panes"
./scripts/pane-properties/list-panes-by-properties.sh --source claude-desktop

echo ""
echo "Tests complete!"
