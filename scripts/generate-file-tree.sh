#!/bin/bash
# generate-file-tree.sh - Generate a file tree structure from a list of file paths
# Usage: echo '["file1", "file2", ...]' | ./generate-file-tree.sh
#        echo '["file1", "file2", ...]' | ./generate-file-tree.sh --root /path/to/project
#        ./generate-file-tree.sh '["file1", "file2", ...]'
#        ./generate-file-tree.sh --root /path/to/project '["file1", "file2", ...]'

# Parse arguments
ROOT_PATH=""
JSON_INPUT=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --root)
            ROOT_PATH="$2"
            shift 2
            ;;
        *)
            JSON_INPUT="$1"
            shift
            ;;
    esac
done

# Function to build and display tree
build_tree() {
    local files=("$@")
    local tree_data=""
    
    # Build tree structure in memory
    declare -A tree_map
    
    for file in "${files[@]}"; do
        # Remove leading/trailing whitespace
        file=$(echo "$file" | xargs)
        
        # Skip empty entries
        [[ -z "$file" ]] && continue
        
        # If root path is specified, make file relative to it
        if [[ -n "$ROOT_PATH" ]]; then
            # Remove root path prefix if present
            file="${file#$ROOT_PATH/}"
            file="${file#$ROOT_PATH}"
        fi
        
        # Add to tree map
        tree_map["$file"]=1
    done
    
    # Sort files for consistent output
    IFS=$'\n' sorted_files=($(printf '%s\n' "${!tree_map[@]}" | sort))
    
    # Process files and build tree
    declare -A dir_structure
    declare -A file_structure
    
    for file in "${sorted_files[@]}"; do
        # Split path into components
        IFS='/' read -ra parts <<< "$file"
        
        # Build directory hierarchy
        current_path=""
        for ((i=0; i<${#parts[@]}-1; i++)); do
            if [[ -n "$current_path" ]]; then
                current_path="$current_path/${parts[$i]}"
            else
                current_path="${parts[$i]}"
            fi
            dir_structure["$current_path"]=1
        done
        
        # Store file with its parent path
        if [[ ${#parts[@]} -gt 1 ]]; then
            parent_path=""
            for ((i=0; i<${#parts[@]}-1; i++)); do
                if [[ -n "$parent_path" ]]; then
                    parent_path="$parent_path/${parts[$i]}"
                else
                    parent_path="${parts[$i]}"
                fi
            done
            file_structure["$parent_path/${parts[-1]}"]="$parent_path"
        else
            # Root level file
            file_structure["$file"]="."
        fi
    done
    
    # Function to print tree recursively
    print_tree_recursive() {
        local path="$1"
        local prefix="$2"
        local is_last="$3"
        
        # Get all direct children (dirs and files)
        local children=()
        local child_dirs=()
        local child_files=()
        
        # Find direct subdirectories
        for dir in "${!dir_structure[@]}"; do
            if [[ "$path" == "." ]]; then
                # Root level - check for no slashes
                if [[ ! "$dir" =~ / ]]; then
                    child_dirs+=("$dir")
                fi
            else
                # Check if direct child
                if [[ "$dir" =~ ^"$path"/[^/]+$ ]]; then
                    local child_name="${dir#$path/}"
                    child_dirs+=("$child_name")
                fi
            fi
        done
        
        # Find direct files
        for file in "${!file_structure[@]}"; do
            if [[ "${file_structure[$file]}" == "$path" ]]; then
                if [[ "$path" == "." ]]; then
                    child_files+=("$file")
                else
                    local child_name="${file#$path/}"
                    child_files+=("$child_name")
                fi
            fi
        done
        
        # Sort directories and files
        IFS=$'\n' child_dirs=($(printf '%s\n' "${child_dirs[@]}" | sort))
        IFS=$'\n' child_files=($(printf '%s\n' "${child_files[@]}" | sort))
        
        # Combine dirs first, then files
        children=("${child_dirs[@]}" "${child_files[@]}")
        
        # Print each child
        local count=0
        local total=${#children[@]}
        
        for child in "${children[@]}"; do
            ((count++))
            local is_last_child=$([[ $count -eq $total ]] && echo "true" || echo "false")
            local connector=$([[ "$is_last_child" == "true" ]] && echo "└── " || echo "├── ")
            local extension=$([[ "$is_last_child" == "true" ]] && echo "    " || echo "│   ")
            
            # Check if it's a directory
            local full_path="$([[ "$path" == "." ]] && echo "$child" || echo "$path/$child")"
            local is_dir=$([[ -n "${dir_structure[$full_path]}" ]] && echo "true" || echo "false")
            
            # Print the item
            echo "${prefix}${connector}${child}$([[ "$is_dir" == "true" ]] && echo "/" || echo "")"
            
            # Recurse for directories
            if [[ "$is_dir" == "true" ]]; then
                print_tree_recursive "$full_path" "${prefix}${extension}" "$is_last_child"
            fi
        done
    }
    
    # If root path is specified, print it as the top level
    if [[ -n "$ROOT_PATH" ]]; then
        # Extract just the directory name from the path
        local root_name=$(basename "$ROOT_PATH")
        echo "$root_name/"
        print_tree_recursive "." "" "true"
    else
        # Start printing from root without a top-level directory
        print_tree_recursive "." "" "true"
    fi
}

# Main script
if [[ -z "$JSON_INPUT" ]]; then
    # Read from stdin if no JSON argument was provided
    JSON_INPUT=$(cat)
fi

# Set json_input for processing
json_input="$JSON_INPUT"

# Parse JSON array using jq
if ! command -v jq &> /dev/null; then
    echo "Error: jq is required but not installed" >&2
    echo "Install with: sudo apt-get install jq" >&2
    exit 1
fi

# Extract files from JSON
files=()
while IFS= read -r file; do
    files+=("$file")
done < <(echo "$json_input" | jq -r '.[]' 2>/dev/null)

# Check if parsing succeeded
if [[ ${#files[@]} -eq 0 ]]; then
    echo "Error: No files found or invalid JSON format" >&2
    echo "Expected format: [\"file1\", \"file2\", ...]" >&2
    exit 1
fi

# Build and display tree
build_tree "${files[@]}"