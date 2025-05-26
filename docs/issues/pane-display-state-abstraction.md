# Feature: Pane Display State Management System

## Problem Statement

Currently, when Cafedelic routes output to panes, it's sending simple text messages that overwrite the pane content. This doesn't properly update application frames (like Emacs) and creates a disconnect between the intended action (open file in Emacs) and what actually displays (just a text message).

We need an abstraction for "upsert pane display state" that can intelligently update pane content based on the target application.

## Current Behavior

When opening a file:
- Expected: Emacs frame updates to show the file
- Actual: Pane shows text "Opened file: filename.ts (X files in context)"

This suggests the output routing is working but not in a way that triggers proper application updates.

## Proposed Solution

Create a display state management system that:

1. **Understands Pane Context**
   - Track what application is running in each pane
   -