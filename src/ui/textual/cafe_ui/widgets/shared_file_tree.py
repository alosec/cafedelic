#!/usr/bin/env python3
"""
SharedFileTree Widget
Legacy wrapper around UnifiedTree for backward compatibility.
"""

from textual.app import ComposeResult
from textual.message import Message
from typing import Optional, Dict, List, Any

from .unified_tree import UnifiedTree


class SharedFileTree(UnifiedTree):
    """Legacy wrapper around UnifiedTree for backward compatibility"""
    
    def __init__(self, context_mode: str = "project", **kwargs):
        # Map legacy context_mode to unified tree mode
        super().__init__(context_mode=context_mode, **kwargs)