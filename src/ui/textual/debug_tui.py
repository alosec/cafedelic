#!/usr/bin/env python3
"""
debug_tui.py - Comprehensive error capture for Cafedelic TUI
Captures all textual errors without requiring GUI launch using console logging.
"""

import sys
import os
import traceback
import asyncio
from io import StringIO
from contextlib import redirect_stderr, redirect_stdout
from typing import List, Dict, Any
from pathlib import Path

# Add project root to Python path
project_root = os.path.join(os.path.dirname(__file__), '..', '..', '..')
sys.path.insert(0, project_root)

try:
    from textual.console import Console
    from textual.app import App
    from textual._context import active_app
    from textual.logging import TextualHandler
    import logging
except ImportError as e:
    print(f"❌ Import error: {e}")
    print("   Make sure virtual environment is activated and textual is installed")
    sys.exit(1)


def setup_comprehensive_logging():
    """Setup logging to capture all textual errors and warnings"""
    # Create console for capturing
    console = Console(stderr=True, force_terminal=False)
    
    # Setup textual logging handler
    textual_handler = TextualHandler(console=console, show_time=False, show_path=False)
    textual_handler.setLevel(logging.DEBUG)
    
    # Setup root logger
    root_logger = logging.getLogger()
    root_logger.setLevel(logging.DEBUG)
    root_logger.addHandler(textual_handler)
    
    # Setup textual specific loggers
    textual_logger = logging.getLogger("textual")
    textual_logger.setLevel(logging.DEBUG)
    
    return console


def capture_app_errors():
    """Attempt to instantiate the app and capture all errors during composition"""
    errors = []
    warnings = []
    
    try:
        # Capture stderr and stdout
        stderr_capture = StringIO()
        stdout_capture = StringIO()
        
        with redirect_stderr(stderr_capture), redirect_stdout(stdout_capture):
            # Import and instantiate the app
            from cafedelic_app import CafedelicApp
            
            print("🔍 Instantiating CafedelicApp...")
            app = CafedelicApp()
            
            print("🔍 Testing app compose...")
            # Try to compose without running
            try:
                # This will trigger widget composition errors
                app._get_default_screen()
            except Exception as compose_error:
                errors.append(f"Compose error: {compose_error}")
                errors.append(f"Traceback: {traceback.format_exc()}")
            
            print("🔍 Testing screen mount simulation...")
            # Try to simulate mounting without actually running
            try:
                screen = app._get_default_screen()
                if hasattr(screen, 'compose'):
                    list(screen.compose())  # Force compose without mount
            except Exception as mount_error:
                errors.append(f"Mount simulation error: {mount_error}")
                errors.append(f"Traceback: {traceback.format_exc()}")
        
        # Capture any stderr/stdout output
        stderr_output = stderr_capture.getvalue()
        stdout_output = stdout_capture.getvalue()
        
        if stderr_output:
            errors.extend(stderr_output.strip().split('\n'))
        if stdout_output and stdout_output.strip():
            warnings.extend(stdout_output.strip().split('\n'))
            
    except ImportError as e:
        errors.append(f"Import error: {e}")
    except Exception as e:
        errors.append(f"General error: {e}")
        errors.append(f"Traceback: {traceback.format_exc()}")
    
    return errors, warnings


async def run_app_with_error_capture():
    """Run the app briefly to capture runtime errors"""
    errors = []
    
    try:
        from cafedelic_app import CafedelicApp
        
        app = CafedelicApp()
        
        # Set up error capturing
        original_excepthook = sys.excepthook
        captured_exceptions = []
        
        def capture_exception(exc_type, exc_value, exc_traceback):
            captured_exceptions.append({
                'type': exc_type.__name__,
                'value': str(exc_value),
                'traceback': ''.join(traceback.format_tb(exc_traceback))
            })
            return original_excepthook(exc_type, exc_value, exc_traceback)
        
        sys.excepthook = capture_exception
        
        # Try to run very briefly
        try:
            # Start the app but exit immediately
            async def quick_exit():
                await asyncio.sleep(0.1)  # Give it time to start
                app.exit()
            
            asyncio.create_task(quick_exit())
            await app.run_async()
            
        except Exception as e:
            errors.append(f"Runtime error: {e}")
            errors.append(f"Traceback: {traceback.format_exc()}")
        finally:
            sys.excepthook = original_excepthook
            
        # Add captured exceptions
        for exc in captured_exceptions:
            errors.append(f"Exception: {exc['type']}: {exc['value']}")
            errors.append(f"Traceback: {exc['traceback']}")
            
    except Exception as e:
        errors.append(f"Async run error: {e}")
        errors.append(f"Traceback: {traceback.format_exc()}")
    
    return errors


def check_virtual_environment():
    """Check virtual environment status"""
    venv_path = Path("venv")
    if not venv_path.exists():
        print("⚠️  Virtual environment not found at ./venv/")
        return False
    
    if hasattr(sys, 'real_prefix') or (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix):
        print("✅ Virtual environment is activated")
        return True
    else:
        print("⚠️  Virtual environment exists but not activated")
        print("   Run: source venv/bin/activate")
        return False


def main():
    """Run comprehensive TUI debugging"""
    print("🐛 Cafedelic TUI Debug Mode")
    print("=" * 40)
    print()
    
    # Check environment
    env_ok = check_virtual_environment()
    if not env_ok:
        print("   Continuing debug anyway...")
    print()
    
    # Setup logging
    console = setup_comprehensive_logging()
    
    all_errors = []
    all_warnings = []
    
    # Phase 1: Static analysis errors
    print("🔍 Phase 1: Capturing composition errors...")
    comp_errors, comp_warnings = capture_app_errors()
    all_errors.extend(comp_errors)
    all_warnings.extend(comp_warnings)
    
    # Phase 2: Runtime errors (if static passes)
    if not comp_errors:
        print("🔍 Phase 2: Capturing runtime errors...")
        try:
            runtime_errors = asyncio.run(run_app_with_error_capture())
            all_errors.extend(runtime_errors)
        except Exception as e:
            all_errors.append(f"Async debug error: {e}")
    
    print()
    print("📊 Debug Results")
    print("=" * 40)
    
    if all_warnings:
        print(f"⚠️  Found {len(all_warnings)} warnings:")
        for i, warning in enumerate(all_warnings, 1):
            print(f"  {i}. {warning}")
        print()
    
    if all_errors:
        print(f"❌ Found {len(all_errors)} errors:")
        for i, error in enumerate(all_errors, 1):
            print(f"  {i}. {error}")
        print()
        print("🔧 Fix these errors to launch the TUI successfully.")
        sys.exit(1)
    else:
        print("✅ No errors detected - TUI should launch successfully")
        print()
        print("💡 Try running: python run.py")
        sys.exit(0)


if __name__ == "__main__":
    main()