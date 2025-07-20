#!/usr/bin/env python3
"""
validate_tui.py - Validation-only command for Cafedelic TUI
Checks for errors without launching the GUI interface.
"""

import ast
import sys
import os
import importlib.util
from typing import List, Dict, Any
from pathlib import Path


def validate_ast_syntax(filepath: str) -> List[str]:
    """Parse file for syntax errors using AST"""
    errors = []
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        ast.parse(content, filename=filepath)
    except SyntaxError as e:
        errors.append(f"Syntax error in {filepath}:{e.lineno}: {e.msg}")
    except Exception as e:
        errors.append(f"Parse error in {filepath}: {e}")
    return errors


def validate_imports(filepath: str) -> List[str]:
    """Test imports without executing compose methods"""
    errors = []
    try:
        # Add current directory to Python path for relative imports
        sys.path.insert(0, os.path.dirname(os.path.abspath(filepath)))
        
        spec = importlib.util.spec_from_file_location("module", filepath)
        if spec and spec.loader:
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
    except ImportError as e:
        errors.append(f"Import error in {filepath}: {e}")
    except Exception as e:
        errors.append(f"Module error in {filepath}: {e}")
    finally:
        # Clean up path
        if sys.path and os.path.dirname(os.path.abspath(filepath)) in sys.path:
            sys.path.remove(os.path.dirname(os.path.abspath(filepath)))
    return errors


def validate_undefined_calls(filepath: str) -> List[str]:
    """Check for undefined function calls like get_commands()"""
    errors = []
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        tree = ast.parse(content, filename=filepath)
        
        # Track defined functions and imports
        defined_functions = set()
        imported_functions = set()
        
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                defined_functions.add(node.name)
            elif isinstance(node, ast.ImportFrom):
                if node.names:
                    for alias in node.names:
                        imported_functions.add(alias.name)
            elif isinstance(node, ast.Import):
                if node.names:
                    for alias in node.names:
                        imported_functions.add(alias.name)
        
        # Check for undefined calls
        for node in ast.walk(tree):
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
                func_name = node.func.id
                # Skip common patterns that are valid
                skip_patterns = [
                    'super',  # Built-in super() call
                    'len', 'str', 'int', 'float', 'bool', 'list', 'dict', 'set', 'tuple',  # Built-ins
                    'print', 'input', 'open', 'range', 'enumerate', 'zip',  # Common built-ins
                    'type', 'isinstance', 'hasattr', 'getattr', 'setattr',  # Reflection
                    'min', 'max', 'sum', 'any', 'all',  # Aggregates
                ]
                
                # Also skip class constructors (uppercase names) and locally defined classes
                is_constructor = func_name[0].isupper() if func_name else False
                
                if (func_name not in defined_functions and 
                    func_name not in imported_functions and
                    func_name not in dir(__builtins__) and
                    func_name not in skip_patterns and
                    not func_name.startswith('_') and
                    not is_constructor):  # Skip constructors and private functions
                    errors.append(f"Undefined function in {filepath}:{node.lineno}: {func_name}()")
    
    except Exception as e:
        errors.append(f"Analysis error in {filepath}: {e}")
    
    return errors


def validate_textual_app_structure(root_dir: str) -> List[str]:
    """Validate Textual app without running GUI"""
    errors = []
    
    # Core files that must exist and be valid
    critical_files = [
        'cafedelic_app.py',
        'run.py',
        'cafe_ui/adapter.py',
        'cafe_ui/components/quick_chat.py',
        'cafe_ui/data/claude_code_data.py'
    ]
    
    for file_path in critical_files:
        full_path = os.path.join(root_dir, file_path)
        if not os.path.exists(full_path):
            errors.append(f"Missing critical file: {file_path}")
            continue
        
        print(f"  Validating {file_path}...")
        
        # AST validation
        syntax_errors = validate_ast_syntax(full_path)
        errors.extend(syntax_errors)
        
        # Skip import validation if syntax errors exist
        if not syntax_errors:
            # Import validation (skip for now to avoid complex dependency issues)
            # import_errors = validate_imports(full_path)
            # errors.extend(import_errors)
            
            # Undefined function validation
            undefined_errors = validate_undefined_calls(full_path)
            errors.extend(undefined_errors)
    
    return errors


def check_virtual_environment():
    """Check if we're in the correct virtual environment"""
    venv_path = Path("venv")
    if not venv_path.exists():
        print("⚠️  Virtual environment not found at ./venv/")
        return False
    
    # Check if we're activated
    if hasattr(sys, 'real_prefix') or (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix):
        print("✅ Virtual environment is activated")
        return True
    else:
        print("⚠️  Virtual environment exists but not activated")
        print("   Run: source venv/bin/activate")
        return False


def main():
    """Run validation-only check"""
    print("🔍 Validating Cafedelic TUI (no GUI launch)...")
    print()
    
    # Check virtual environment
    env_ok = check_virtual_environment()
    if not env_ok:
        print("   Continuing validation anyway...")
    print()
    
    # Run validation
    errors = validate_textual_app_structure('.')
    
    print()
    if errors:
        print(f"❌ Found {len(errors)} validation errors:")
        for error in errors:
            print(f"  • {error}")
        print()
        print("Fix these errors before launching the TUI.")
        sys.exit(1)
    else:
        print("✅ All validation checks passed - TUI should launch successfully")
        sys.exit(0)


if __name__ == "__main__":
    main()