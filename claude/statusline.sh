#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract values from JSON
model_name=$(echo "$input" | jq -r '.model.display_name // "Claude"')
current_dir=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
project_dir=$(echo "$input" | jq -r '.workspace.project_dir // ""')
output_style=$(echo "$input" | jq -r '.output_style.name // "default"')

# Colors matching zsh prompt
marker_color=209
env_color=203
host_color=255
path_color=117
fade_color=245
branch_color=150
dirty_color=215
time_color=250

# Get relative path from project root or use ~ for home
if [ -n "$project_dir" ] && [[ "$current_dir" == "$project_dir"* ]]; then
    if [ "$current_dir" = "$project_dir" ]; then
        path_display="."
    else
        path_display=".${current_dir#$project_dir}"
    fi
elif [[ "$current_dir" == "$HOME"* ]]; then
    path_display="~${current_dir#$HOME}"
else
    path_display="$current_dir"
fi

# Check if in git repo and get branch info
git_info=""
if [ -d "$current_dir/.git" ] || git -C "$current_dir" rev-parse --git-dir > /dev/null 2>&1; then
    branch=$(git -C "$current_dir" branch --show-current 2>/dev/null)
    if [ -n "$branch" ]; then
        # Check if repo is dirty
        if ! git -C "$current_dir" diff-index --quiet HEAD 2>/dev/null; then
            git_info=$(printf " \033[38;5;%dm \033[38;5;%dm%s*\033[0m" "$branch_color" "$dirty_color" "$branch")
        else
            git_info=$(printf " \033[38;5;%dm %s\033[0m" "$branch_color" "$branch")
        fi
    fi
fi

# Check for Guix environment
env_indicator=""
if [ -n "$GUIX_ENVIRONMENT" ]; then
    marker_color=$env_color
fi

# Get hostname (use multiple methods for reliability)
hostname=$(cat /proc/sys/kernel/hostname 2>/dev/null || uname -n 2>/dev/null || hostname -s 2>/dev/null || echo "unknown")

# Build the status line
printf "\033[38;5;%dm\u03bb\033[0m " "$marker_color"  # Lambda symbol
printf "\033[38;5;%dm%s\033[0m " "$host_color" "$hostname"
printf "\033[38;5;%dm%s\033[0m" "$path_color" "$path_display"
printf "%s" "$git_info"
printf " \033[38;5;%dm›\033[0m " "$fade_color"  # Prompt marker
printf "\033[38;5;%dm%s\033[0m" "$time_color" "$model_name"

if [ "$output_style" != "default" ]; then
    printf " \033[38;5;%dm(%s)\033[0m" "$fade_color" "$output_style"
fi
