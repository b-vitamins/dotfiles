#!/usr/bin/env bash
# setup.sh - Dotfiles installation and configuration manager
#
# This script manages the installation of configuration files from this
# dotfiles repository to their appropriate locations in the home directory.
# It follows XDG Base Directory specifications and supports machine-specific
# configurations.
set -euo pipefail
# Script metadata
SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_NAME
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
readonly SCRIPT_DIR
readonly DOTFILES_DIR="$SCRIPT_DIR"
# Color codes for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly BOLD='\033[1m'
readonly RESET='\033[0m'
# Default configuration
DRY_RUN=false
FORCE=false
VERBOSE=false
MACHINE=""
GUIX_ONLY=false
SKIP_TEMPLATES=false
SKIP_HOOKS=false
CLAUDE_SYMLINK=false
CLAUDE_UPDATE=false
CLAUDE_AGENTS=""
EXCLUDE_AGENTS=""
SHOW_STATUS=false
BACKUP_ON_OVERWRITE=true
QUICK_MODE=false
PROVISION_ROOT_SECRETS=false
AUTO_PROVISION_ROOT_SECRETS=false
readonly DEFAULT_SSH_PASS_ENTRY="keys/ssh/freydis/id_ed25519"
readonly DEFAULT_SSH_PUBLIC_PASS_ENTRY="keys/ssh/freydis/id_ed25519.pub"
declare -a SETUP_BLOCKERS=()
declare -a SETUP_WARNINGS=()
declare -a SETUP_NOTES=()
# -----------------------------------------------------------------------------
# Utility functions
# -----------------------------------------------------------------------------
log() {
	local level="$1"
	shift
	local message="$*"
	local color=""
	case "$level" in
	ERROR) color="$RED" ;;
	SUCCESS) color="$GREEN" ;;
	WARNING) color="$YELLOW" ;;
	INFO) color="$BLUE" ;;
	*) color="$RESET" ;;
	esac
	printf "%b%b[%s]%b %s\n" "$color" "$BOLD" "$level" "$RESET" "$message" >&2
}
die() {
	log ERROR "$@"
	exit 1
}
debug() {
	if [[ "$VERBOSE" == true ]]; then
		log INFO "$@"
	fi
}
record_setup_issue() {
	local kind="$1"
	local summary="$2"
	local action="${3:-}"
	local entry="$summary"
	local -n bucket="$kind"
	if [[ -n "$action" ]]; then
		entry+="|$action"
	fi
	bucket+=("$entry")
}
clear_setup_issues() {
	SETUP_BLOCKERS=()
	SETUP_WARNINGS=()
	SETUP_NOTES=()
}
print_setup_issue_group() {
	local title="$1"
	local -n bucket="$2"
	local entry
	local summary
	local action
	[[ ${#bucket[@]} -gt 0 ]] || return 0
	echo "$title:"
	for entry in "${bucket[@]}"; do
		summary="${entry%%|*}"
		action=""
		if [[ "$entry" == *"|"* ]]; then
			action="${entry#*|}"
		fi
		echo "  - $summary"
		if [[ -n "$action" ]]; then
			echo "    Action: $action"
		fi
	done
	echo
}
print_setup_action_report() {
	if [[ ${#SETUP_BLOCKERS[@]} -eq 0 ]] &&
		[[ ${#SETUP_WARNINGS[@]} -eq 0 ]] &&
		[[ ${#SETUP_NOTES[@]} -eq 0 ]]; then
		debug "No action items detected during preflight"
		return 0
	fi
	log INFO "Action Report"
	echo "============="
	print_setup_issue_group "Blocking Items" SETUP_BLOCKERS
	print_setup_issue_group "Warnings" SETUP_WARNINGS
	print_setup_issue_group "Manual Follow-Ups" SETUP_NOTES
}
xdg_data_home() {
	printf '%s\n' "${XDG_DATA_HOME:-$HOME/.local/share}"
}
confirm() {
	local prompt="$1"
	local response
	if [[ "$FORCE" == true ]] || [[ "$QUICK_MODE" == true ]]; then
		return 0
	fi
	printf "%b%b[?]%b %s [y/N] " "$YELLOW" "$BOLD" "$RESET" "$prompt"
	read -r response
	[[ "$response" =~ ^[Yy]$ ]]
}
# -----------------------------------------------------------------------------
# Usage and help
# -----------------------------------------------------------------------------
usage() {
	cat <<EOF
${BOLD}NAME${RESET}
    $SCRIPT_NAME - Dotfiles installation and configuration manager
${BOLD}SYNOPSIS${RESET}
    $SCRIPT_NAME [OPTIONS]
${BOLD}DESCRIPTION${RESET}
    Installs configuration files from this dotfiles repository to their
    appropriate locations. Supports dry-run mode, machine-specific configs,
    and selective installation.
${BOLD}OPTIONS${RESET}
    -d, --dry-run
        Show what would be done without making changes
    -f, --force
        Overwrite existing files without prompting
    -v, --verbose
        Enable verbose output
    -m, --machine HOSTNAME
        Use configuration for specified machine (default: current hostname)
    -g, --guix-only
        Only install Guix-related configurations
    -s, --skip-templates
        Skip Emacs template regeneration
    -k, --skip-hooks
        Skip Git hooks installation
    --claude-symlink
        Use symlinks for Claude configs instead of copying
    --claude-update
        Update existing Claude configs (shows diff, prompts for each file)
    --claude-agents PATTERN
        Install only agents matching pattern (e.g., "python-*,latex-*")
    --exclude-agents PATTERN
        Exclude agents matching pattern (e.g., "typescript-*")
    --status [component]
        Show status of installed configs (all, claude, emacs, guix)
    --no-backup
        Don't create backups when overwriting files
    --provision-root-secrets
        Force machine-specific root credential provisioning from pass using sudo
    -q, --quick
        Quick mode - skip all prompts, assume yes (for experienced users)
    -h, --help
        Display this help message
${BOLD}EXAMPLES${RESET}
    # Preview changes without applying them
    $SCRIPT_NAME --dry-run
    # Force installation, overwriting existing files
    $SCRIPT_NAME --force
    # Install only Guix configurations for a specific machine
    $SCRIPT_NAME --guix-only --machine mileva
    # Check status of installed configurations
    $SCRIPT_NAME --status
    # Update existing Claude configs interactively
    $SCRIPT_NAME --claude-update
    # Use symlinks for Claude configs
    $SCRIPT_NAME --claude-symlink
    # Install only Python and LaTeX agents
    $SCRIPT_NAME --claude-agents "python-*,latex-*"
    # Re-run only the root credential provisioning logic
    $SCRIPT_NAME --machine mileva --guix-only --provision-root-secrets
${BOLD}SUPPORTED CONFIGURATIONS${RESET}
    Managed by this script:
    - Emacs (init files, lisp modules, templates)
    - Guix System (channels.scm only)
    - Codex managed instructions/rules + skills (keeps ~/.codex/config.toml local)
    - Claude Code (settings, agents, hooks, instructions, output styles, project templates)
    - qBittorrent configuration (if present)
    - Automatic bootstrap of default SSH identity and machine-specific root credentials when available
    - Git hooks installation
    Managed by Guix home configuration:
    - Shell (zsh configuration)
    - Git (config, ignore patterns)
    - Terminal (alacritty)
    - Media (mpv with shaders)
    - Development tools (grobid via containers)
EOF
}
# -----------------------------------------------------------------------------
# Argument parsing
# -----------------------------------------------------------------------------
parse_arguments() {
	while [[ $# -gt 0 ]]; do
		case "$1" in
		-d | --dry-run)
			DRY_RUN=true
			shift
			;;
		-f | --force)
			FORCE=true
			shift
			;;
		-v | --verbose)
			VERBOSE=true
			shift
			;;
		-m | --machine)
			MACHINE="$2"
			shift 2
			;;
		-g | --guix-only)
			GUIX_ONLY=true
			shift
			;;
		-s | --skip-templates)
			SKIP_TEMPLATES=true
			shift
			;;
		-k | --skip-hooks)
			SKIP_HOOKS=true
			shift
			;;
		--claude-symlink)
			CLAUDE_SYMLINK=true
			shift
			;;
		--claude-update)
			CLAUDE_UPDATE=true
			shift
			;;
		--claude-agents)
			CLAUDE_AGENTS="$2"
			shift 2
			;;
		--exclude-agents)
			EXCLUDE_AGENTS="$2"
			shift 2
			;;
		--status)
			SHOW_STATUS=true
			if [[ $# -gt 1 ]] && [[ -n "$2" ]] && [[ "$2" != -* ]]; then
				STATUS_COMPONENT="$2"
				shift 2
			else
				STATUS_COMPONENT="all"
				shift
			fi
			;;
			--no-backup)
				BACKUP_ON_OVERWRITE=false
				shift
				;;
			--provision-root-secrets)
				PROVISION_ROOT_SECRETS=true
				shift
				;;
			-q | --quick)
				QUICK_MODE=true
				shift
			;;
		-h | --help)
			usage
			exit 0
			;;
		*)
			die "Unknown option: $1\nRun '$SCRIPT_NAME --help' for usage"
			;;
		esac
	done
}
# -----------------------------------------------------------------------------
# Configuration detection
# -----------------------------------------------------------------------------
detect_environment() {
	# Determine hostname
	if [[ -z "$MACHINE" ]]; then
		MACHINE="$(hostname)"
		debug "Detected hostname: $MACHINE"
	else
		log INFO "Using specified machine: $MACHINE"
	fi
	# Check if we're in a Git repository
	if [[ -d "$DOTFILES_DIR/.git" ]]; then
		debug "Git repository detected"
		readonly IS_GIT_REPO=true
	else
		debug "Not a Git repository"
		readonly IS_GIT_REPO=false
	fi
	# Check for Emacs
	if command -v emacs >/dev/null 2>&1; then
		debug "Emacs found: $(command -v emacs)"
		readonly HAS_EMACS=true
	else
		log WARNING "Emacs not found - template generation will be skipped"
		readonly HAS_EMACS=false
	fi
	# Check for Claude Code
	if command -v claude >/dev/null 2>&1; then
		debug "Claude Code found: $(command -v claude)"
		readonly HAS_CLAUDE=true
	else
		debug "Claude Code not found"
		readonly HAS_CLAUDE=false
	fi
}
# -----------------------------------------------------------------------------
# Backup functionality
# -----------------------------------------------------------------------------
create_backup() {
	local file="$1"
	if [[ ! -e "$file" ]]; then
		return 0
	fi
	if [[ "$BACKUP_ON_OVERWRITE" == false ]]; then
		return 0
	fi
	local backup_name="${file}.backup.$(date +%Y%m%d_%H%M%S)"
	if [[ "$DRY_RUN" == true ]]; then
		echo "  Would backup: $file -> $backup_name"
	else
		cp -r "$file" "$backup_name" && {
			log SUCCESS "Created backup: $backup_name"
			# Track backups for potential rollback
			echo "$backup_name:$file" >>"$DOTFILES_DIR/.setup_backups.tmp"
		} || {
			log WARNING "Failed to create backup: $backup_name"
		}
	fi
}
rollback_on_error() {
	if [[ ! -f "$DOTFILES_DIR/.setup_backups.tmp" ]]; then
		return 0
	fi
	log WARNING "Rolling back changes due to error..."
	while IFS=: read -r backup original; do
		if [[ -e "$backup" ]]; then
			mv "$backup" "$original" && {
				log SUCCESS "Restored: $original"
			} || {
				log ERROR "Failed to restore: $original"
			}
		fi
	done <"$DOTFILES_DIR/.setup_backups.tmp"
	rm -f "$DOTFILES_DIR/.setup_backups.tmp"
}
# -----------------------------------------------------------------------------
# Status display
# -----------------------------------------------------------------------------
show_status() {
	local component="${1:-all}"
	case "$component" in
	claude | all)
		show_claude_status
		[[ "$component" == "claude" ]] && return
		echo
		;;
	esac
	case "$component" in
	emacs | all)
		show_emacs_status
		[[ "$component" == "emacs" ]] && return
		echo
		;;
	esac
	case "$component" in
	guix | all)
		show_guix_status
		;;
	*)
		die "Unknown component: $component (valid: all, claude, emacs, guix)"
		;;
	esac
}
show_claude_status() {
	log INFO "Claude Code Configuration Status"
	echo "================================="
	if [[ "$HAS_CLAUDE" == true ]]; then
		echo "Claude Code: $(command -v claude)"
	else
		echo "Claude Code: NOT INSTALLED"
	fi
	echo
	echo "Configuration files:"
	# Check main config files
	local claude_files=("CLAUDE.md" "settings.json" "settings.local.json" "statusline.sh")
	for file in "${claude_files[@]}"; do
		local repo_file="$DOTFILES_DIR/claude/$file"
		local home_file="$HOME/.claude/$file"
		printf "  %-20s " "$file:"
		if [[ -f "$home_file" ]]; then
			if [[ -f "$repo_file" ]]; then
				if diff -q "$repo_file" "$home_file" >/dev/null 2>&1; then
					printf "${GREEN}✓ installed (up to date)${RESET}\n"
				else
					printf "${YELLOW}✓ installed (differs from repo)${RESET}\n"
				fi
			else
				printf "${YELLOW}✓ installed (not in repo)${RESET}\n"
			fi
		else
			if [[ -f "$repo_file" ]]; then
				printf "${RED}✗ not installed${RESET}\n"
			else
				printf "${BLUE}- not available${RESET}\n"
			fi
		fi
	done
	# Check infrastructure directories
	echo
	echo "Infrastructure directories:"
	local claude_dirs=("hooks" "instructions" "output-styles" "project-templates")
	for dir in "${claude_dirs[@]}"; do
		local repo_dir="$DOTFILES_DIR/claude/$dir"
		local home_dir="$HOME/.claude/$dir"
		printf "  %-20s " "$dir:"
		if [[ -d "$home_dir" ]]; then
			if [[ -d "$repo_dir" ]]; then
				local repo_count=$(find "$repo_dir" -type f | wc -l 2>/dev/null || echo 0)
				local home_count=$(find "$home_dir" -type f | wc -l 2>/dev/null || echo 0)
				if [[ $repo_count -eq $home_count ]]; then
					printf "${GREEN}✓ installed ($home_count files)${RESET}\n"
				else
					printf "${YELLOW}✓ installed ($home_count/$repo_count files)${RESET}\n"
				fi
			else
				printf "${YELLOW}✓ installed (not in repo)${RESET}\n"
			fi
		else
			if [[ -d "$repo_dir" ]]; then
				local repo_count=$(find "$repo_dir" -type f | wc -l 2>/dev/null || echo 0)
				printf "${RED}✗ not installed ($repo_count files available)${RESET}\n"
			else
				printf "${BLUE}- not available${RESET}\n"
			fi
		fi
	done
	# Check agents
	echo
	echo "Agents:"
	local installed_count=0
	local available_count=0
	local stale_count=0
	if [[ -d "$DOTFILES_DIR/claude/agents" ]]; then
		available_count=$(find "$DOTFILES_DIR/claude/agents" -name "*.md" -type f | wc -l)
	fi
	if [[ -d "$HOME/.claude/agents" ]]; then
		installed_count=$(find "$HOME/.claude/agents" -name "*.md" -type f | wc -l)
		# Check for stale agents
		for agent in "$HOME/.claude/agents"/*.md; do
			[[ -f "$agent" ]] || continue
			local agent_name="$(basename "$agent")"
			if [[ ! -f "$DOTFILES_DIR/claude/agents/$agent_name" ]]; then
				stale_count=$((stale_count + 1))
			fi
		done
	fi
	echo "  Available in repo: $available_count"
	echo "  Installed: $installed_count"
	[[ $stale_count -gt 0 ]] && echo "  ${YELLOW}Stale (not in repo): $stale_count${RESET}"
}
show_emacs_status() {
	log INFO "Emacs Configuration Status"
	echo "=========================="
	if [[ "$HAS_EMACS" == true ]]; then
		echo "Emacs: $(command -v emacs)"
	else
		echo "Emacs: NOT INSTALLED"
	fi
	echo
	echo "Configuration files:"
	# Check Emacs config files
	local emacs_items=("init.el" "early-init.el" "lisp" "templates" "themes")
	for item in "${emacs_items[@]}"; do
		local repo_item="$DOTFILES_DIR/emacs/$item"
		local home_item="$HOME/.config/emacs/$item"
		printf "  %-20s " "$item:"
		if [[ -e "$home_item" ]]; then
			if [[ -L "$home_item" ]]; then
				if [[ "$(readlink -f "$home_item")" == "$(readlink -f "$repo_item")" ]]; then
					printf "${GREEN}✓ linked correctly${RESET}\n"
				else
					printf "${YELLOW}✓ linked (to different location)${RESET}\n"
				fi
			else
				printf "${YELLOW}✓ exists (not linked)${RESET}\n"
			fi
		else
			printf "${RED}✗ not installed${RESET}\n"
		fi
	done
}
show_guix_status() {
	log INFO "Guix Configuration Status"
	echo "========================"
	echo "Machine: $MACHINE"
	if [[ -f "$DOTFILES_DIR/guix/machines/$MACHINE.scm" ]]; then
		echo "Machine config: ${GREEN}✓ available${RESET}"
	else
		echo "Machine config: ${RED}✗ not found${RESET}"
	fi
	echo
	echo "Configuration files:"
	local guix_files=("channels.scm" "config.scm")
	for file in "${guix_files[@]}"; do
		local home_file="$HOME/.config/guix/$file"
		printf "  %-20s " "$file:"
		if [[ -L "$home_file" ]]; then
			printf "${GREEN}✓ linked${RESET}\n"
		elif [[ -f "$home_file" ]]; then
			printf "${YELLOW}✓ exists (not linked)${RESET}\n"
		else
			printf "${RED}✗ not installed${RESET}\n"
		fi
	done
}
# -----------------------------------------------------------------------------
# Agent filtering
# -----------------------------------------------------------------------------
should_install_agent() {
	local agent_name="$1"
	# If specific agents requested, check if this matches
	if [[ -n "$CLAUDE_AGENTS" ]]; then
		local matched=false
		IFS=',' read -ra PATTERNS <<<"$CLAUDE_AGENTS"
		for pattern in "${PATTERNS[@]}"; do
			# Use glob pattern matching
			if [[ "$agent_name" == ${pattern} ]]; then
				matched=true
				break
			fi
		done
		[[ "$matched" == false ]] && return 1
	fi
	# Check exclusions
	if [[ -n "$EXCLUDE_AGENTS" ]]; then
		IFS=',' read -ra PATTERNS <<<"$EXCLUDE_AGENTS"
		for pattern in "${PATTERNS[@]}"; do
			# Use glob pattern matching
			if [[ "$agent_name" == ${pattern} ]]; then
				return 1
			fi
		done
	fi
	return 0
}
# -----------------------------------------------------------------------------
# Claude update functionality
# -----------------------------------------------------------------------------
update_claude_config() {
	log INFO "Updating Claude Code configuration..."
	if [[ ! -d "$HOME/.claude" ]]; then
		log WARNING "No existing Claude configuration found"
		return 1
	fi
	local updated=0
	local skipped=0
	# Update main config files
	local claude_files=("CLAUDE.md" "settings.json" "settings.local.json" "statusline.sh")
	for file in "${claude_files[@]}"; do
		update_claude_file "$file" "$HOME/.claude/$file" && updated=$((updated + 1)) || skipped=$((skipped + 1))
	done
	# Update agents
	if [[ -d "$DOTFILES_DIR/claude/agents" ]]; then
		for agent_file in "$DOTFILES_DIR/claude/agents"/*.md; do
			if [[ -f "$agent_file" ]]; then
				local agent_name="$(basename "$agent_file")"
				if ! should_install_agent "${agent_name%.md}"; then
					debug "Skipping excluded agent: $agent_name"
					continue
				fi
				update_claude_file "agents/$agent_name" "$HOME/.claude/agents/$agent_name" && updated=$((updated + 1)) || skipped=$((skipped + 1))
			fi
		done
	fi
	# Update infrastructure directories
	local claude_dirs=("hooks" "instructions" "output-styles" "project-templates")
	for dir in "${claude_dirs[@]}"; do
		if [[ -d "$DOTFILES_DIR/claude/$dir" ]]; then
			for source_file in "$DOTFILES_DIR/claude/$dir"/*; do
				if [[ -f "$source_file" ]]; then
					local file_name="$(basename "$source_file")"
					local target_file="$HOME/.claude/$dir/$file_name"
					update_claude_file "$dir/$file_name" "$target_file" && updated=$((updated + 1)) || skipped=$((skipped + 1))
				fi
			done
		fi
	done
	# Clean stale agents
	clean_stale_agents
	echo
	log INFO "Update complete: $updated files updated, $skipped skipped"
}
update_claude_file() {
	local relative_path="$1"
	local target_path="$2"
	local source_path="$DOTFILES_DIR/claude/$relative_path"
	if [[ ! -f "$source_path" ]]; then
		return 1
	fi
	if [[ ! -f "$target_path" ]]; then
		log INFO "New file: $relative_path"
		if confirm "Install new file: $relative_path?"; then
			if [[ "$DRY_RUN" == false ]]; then
				mkdir -p "$(dirname "$target_path")"
				cp "$source_path" "$target_path"
				log SUCCESS "Installed: $relative_path"
			else
				echo "  Would install: $relative_path"
			fi
			return 0
		fi
		return 1
	fi
	# Check if files differ
	if diff -q "$source_path" "$target_path" >/dev/null 2>&1; then
		debug "Up to date: $relative_path"
		return 1
	fi
	# Show diff
	log INFO "Changes in: $relative_path"
	echo "----------------------------------------"
	diff -u "$target_path" "$source_path" | head -20 || true
	echo "----------------------------------------"
	if confirm "Update file: $relative_path?"; then
		if [[ "$DRY_RUN" == false ]]; then
			create_backup "$target_path"
			cp "$source_path" "$target_path"
			log SUCCESS "Updated: $relative_path"
		else
			echo "  Would update: $relative_path"
		fi
		return 0
	fi
	return 1
}
clean_stale_agents() {
	if [[ ! -d "$HOME/.claude/agents" ]]; then
		return 0
	fi
	local stale_count=0
	for agent in "$HOME/.claude/agents"/*.md; do
		[[ -f "$agent" ]] || continue
		local agent_name="$(basename "$agent")"
		if [[ ! -f "$DOTFILES_DIR/claude/agents/$agent_name" ]]; then
			log WARNING "Stale agent found: $agent_name"
			stale_count=$((stale_count + 1))
			if confirm "Remove stale agent: $agent_name?"; then
				if [[ "$DRY_RUN" == false ]]; then
					rm "$agent"
					log SUCCESS "Removed: $agent_name"
				else
					echo "  Would remove: $agent_name"
				fi
			fi
		fi
	done
	if [[ $stale_count -eq 0 ]]; then
		debug "No stale agents found"
	fi
}
# -----------------------------------------------------------------------------
# Link definitions
# -----------------------------------------------------------------------------
declare -A LINKS=()
declare -A COPIES=()
define_links() {
	# Only add non-Guix links if not in guix-only mode
	if [[ "$GUIX_ONLY" == false ]]; then
		# Emacs configuration - ONLY symlink these as they're not in Guix config
		LINKS["$DOTFILES_DIR/emacs/init.el"]="$HOME/.config/emacs/init.el"
		LINKS["$DOTFILES_DIR/emacs/early-init.el"]="$HOME/.config/emacs/early-init.el"
		LINKS["$DOTFILES_DIR/emacs/lisp"]="$HOME/.config/emacs/lisp"
		LINKS["$DOTFILES_DIR/emacs/setup"]="$HOME/.config/emacs/setup"
		LINKS["$DOTFILES_DIR/emacs/templates"]="$HOME/.config/emacs/templates"
		LINKS["$DOTFILES_DIR/emacs/themes"]="$HOME/.config/emacs/themes"
		LINKS["$DOTFILES_DIR/emacs/elfeed.score"]="$HOME/.config/emacs/elfeed.score"
		# Claude Code configuration
		if [[ -d "$DOTFILES_DIR/claude" ]]; then
			setup_claude_config
		fi
		# Note: The following are handled by Guix home configuration:
		# - Shell (zsh/zshrc, zsh/zshenv)
		# - Git (git/gitconfig, git/gitignore)
		# - Terminal (alacritty/alacritty.toml)
		# - Media (mpv/mpv.conf, mpv/input.conf, mpv/shaders)
		# - Development tools (grobid via oci-container)
		# DO NOT create symlinks for these!
		# qBittorrent is not managed by Guix, so we handle it here
		# We copy instead of symlink because qBittorrent modifies config files
		if [[ -d "$DOTFILES_DIR/qBittorrent" ]]; then
			COPIES["$DOTFILES_DIR/qBittorrent"]="$HOME/.config/qBittorrent"
		fi
		# Setup executables in ~/.local/bin (XDG compliant location)
		if [[ -d "$DOTFILES_DIR/bin" ]]; then
			setup_user_bin
		fi
		# Setup Codex managed files + skills from dotfiles repo
		setup_codex_config
		setup_codex_skills
	fi
	# Guix configuration (always included unless machine not found)
	if [[ -d "$DOTFILES_DIR/guix" ]]; then
		if [[ -f "$DOTFILES_DIR/guix/machines/$MACHINE.scm" ]]; then
			LINKS["$DOTFILES_DIR/guix/channels.scm"]="$HOME/.config/guix/channels.scm"
			LINKS["$DOTFILES_DIR/guix/machines/$MACHINE.scm"]="$HOME/.config/guix/config.scm"
		else
			log WARNING "No Guix configuration found for machine: $MACHINE"
			log INFO "Available machines:"
			local machines=()
			for machine in "$DOTFILES_DIR"/guix/machines/*.scm; do
				[[ -f "$machine" ]] && machines+=("$(basename "$machine" .scm)")
			done
			for m in "${machines[@]}"; do
				echo "  - $m"
			done
			# Auto-suggest if only one machine available
			if [[ ${#machines[@]} -eq 1 ]]; then
				log INFO "Auto-detecting single available machine: ${machines[0]}"
				if confirm "Use machine configuration: ${machines[0]}?"; then
					MACHINE="${machines[0]}"
					LINKS["$DOTFILES_DIR/guix/channels.scm"]="$HOME/.config/guix/channels.scm"
					LINKS["$DOTFILES_DIR/guix/machines/${machines[0]}.scm"]="$HOME/.config/guix/config.scm"
				fi
			fi
		fi
	fi
}
ensure_codex_local_config() {
	local source_codex_dir="$DOTFILES_DIR/codex"
	local target_codex_dir="$HOME/.codex"
	local source_config="$source_codex_dir/config.toml"
	local target_config="$target_codex_dir/config.toml"

	[[ -f "$source_config" ]] || return 0

	if [[ ! -d "$target_codex_dir" ]]; then
		debug "Creating Codex config directory: $target_codex_dir"
		if [[ "$DRY_RUN" == false ]]; then
			mkdir -p "$target_codex_dir"
		fi
	fi

	if [[ -L "$target_config" ]] && [[ "$(readlink -f "$target_config")" == "$(readlink -f "$source_config")" ]]; then
		if [[ "$DRY_RUN" == true ]]; then
			echo "  Would decouple Codex local config from dotfiles: $target_config"
		else
			local tmp_config
			tmp_config="$(mktemp "$target_codex_dir/config.toml.tmp.XXXXXX")"
			cp "$target_config" "$tmp_config"
			rm -f "$target_config"
			mv "$tmp_config" "$target_config"
			chmod 600 "$target_config" 2>/dev/null || true
			log SUCCESS "Decoupled Codex local config from dotfiles: $target_config"
		fi
		return 0
	fi

	if [[ ! -e "$target_config" && ! -L "$target_config" ]]; then
		if [[ "$DRY_RUN" == true ]]; then
			echo "  Would create local Codex config from template: $target_config"
		else
			cp "$source_config" "$target_config"
			chmod 600 "$target_config" 2>/dev/null || true
			log SUCCESS "Created local Codex config from template: $target_config"
		fi
	fi
}
# -----------------------------------------------------------------------------
# Codex config setup
# -----------------------------------------------------------------------------
setup_codex_config() {
	local source_codex_dir="$DOTFILES_DIR/codex"
	local target_codex_dir="$HOME/.codex"
	local queued=0
	local relative_path

	if [[ ! -d "$source_codex_dir" ]]; then
		debug "No Codex directory found at: $source_codex_dir"
		return 0
	fi

	# Keep Codex runtime state local (do not symlink config.toml from dotfiles).
	ensure_codex_local_config

	# Track only stable, user-authored Codex files.
	for relative_path in "instructions.md"; do
		local source_file="$source_codex_dir/$relative_path"
		local target_file="$target_codex_dir/$relative_path"

		[[ -f "$source_file" ]] || continue

		if [[ -L "$target_file" ]] && [[ "$(readlink -f "$target_file")" == "$(readlink -f "$source_file")" ]]; then
			debug "Codex file already linked: $relative_path"
			continue
		fi

		LINKS["$source_file"]="$target_file"
		queued=$((queued + 1))
	done

	# Optional custom rules owned by dotfiles (exclude generated default.rules).
	local source_rules_dir="$source_codex_dir/rules"
	if [[ -d "$source_rules_dir" ]]; then
		while IFS= read -r -d '' rule_file; do
			local rule_name
			local target_rule
			rule_name="$(basename "$rule_file")"
			target_rule="$target_codex_dir/rules/$rule_name"

			if [[ -L "$target_rule" ]] && [[ "$(readlink -f "$target_rule")" == "$(readlink -f "$rule_file")" ]]; then
				debug "Codex rule already linked: $rule_name"
				continue
			fi

			LINKS["$rule_file"]="$target_rule"
			queued=$((queued + 1))
		done < <(find "$source_rules_dir" -maxdepth 1 -type f -name "*.rules" ! -name "default.rules" -print0 | sort -z)
	fi

	if [[ $queued -gt 0 ]]; then
		log INFO "Queued $queued Codex managed symlink(s) for installation"
	else
		debug "No Codex managed symlinks needed"
	fi
}
# -----------------------------------------------------------------------------
# Codex skills setup
# -----------------------------------------------------------------------------
setup_codex_skills() {
	local source_skills_dir="$DOTFILES_DIR/codex/skills"
	local target_skills_dir="$HOME/.codex/skills"
	local queued=0
	local skill_dir
	if [[ ! -d "$source_skills_dir" ]]; then
		debug "No Codex skills directory found at: $source_skills_dir"
		return 0
	fi
	for skill_dir in "$source_skills_dir"/*; do
		[[ -d "$skill_dir" ]] || continue
		[[ -f "$skill_dir/SKILL.md" ]] || {
			debug "Skipping non-skill directory: $(basename "$skill_dir")"
			continue
		}
		local skill_name
		local target_link
		skill_name="$(basename "$skill_dir")"
		target_link="$target_skills_dir/$skill_name"
		if [[ -L "$target_link" ]] && [[ "$(readlink -f "$target_link")" == "$(readlink -f "$skill_dir")" ]]; then
			debug "Codex skill already linked: $skill_name"
			continue
		fi
		# Only auto-install missing skill links. Keep any existing target untouched.
		if [[ -e "$target_link" || -L "$target_link" ]]; then
			log WARNING "Codex skill target exists, skipping: $target_link"
			continue
		fi
		LINKS["$skill_dir"]="$target_link"
		queued=$((queued + 1))
	done
	if [[ $queued -gt 0 ]]; then
		log INFO "Queued $queued Codex skill symlink(s) for installation"
	else
		debug "No new Codex skill symlinks needed"
	fi
}
# -----------------------------------------------------------------------------
# User bin setup
# -----------------------------------------------------------------------------
setup_user_bin() {
	local user_bin_dir="$HOME/.local/bin"
	local dotfiles_bin_dir="$DOTFILES_DIR/bin"
	debug "Setting up user bin directory..."
	# Create ~/.local/bin if it doesn't exist
	if [[ ! -d "$user_bin_dir" ]]; then
		debug "Creating user bin directory: $user_bin_dir"
		if [[ "$DRY_RUN" == false ]]; then
			mkdir -p "$user_bin_dir"
		fi
	fi
	# Find all files in bin directory (excluding cache and hidden files)
	local bin_count=0
	while IFS= read -r -d '' bin_file; do
		local bin_name="$(basename "$bin_file")"
		# Skip cache files, compiled files, and non-executable files
		# Match patterns from .gitignore
		if [[ "$bin_name" == *.pyc ]] ||
			[[ "$bin_name" == *.pyo ]] ||
			[[ "$bin_name" == *.pyd ]] ||
			[[ "$bin_name" == *'$py.class' ]] ||
			[[ "$bin_name" == *.so ]] ||
			[[ "$bin_name" == *.o ]] ||
			[[ "$bin_name" == *.a ]] ||
			[[ "$bin_name" == *.elc ]] ||
			[[ "$bin_name" == CACHEDIR.TAG ]] ||
			[[ "$bin_name" =~ ^[0-9]+$ ]] ||
			[[ "$bin_name" =~ ^\..* ]] ||
			[[ "$bin_name" == *~ ]] ||
			[[ "$bin_name" == *.swp ]] ||
			[[ "$bin_name" == *.bak ]] ||
			[[ "$bin_name" == *.tmp ]] ||
			[[ ! -x "$bin_file" ]]; then
			debug "Skipping non-executable or cache file: $bin_name"
			continue
		fi
		# Remove extension for cleaner command names (only for .py files)
		local link_name="$bin_name"
		if [[ "$bin_name" == *.py ]]; then
			link_name="${bin_name%.py}"
		fi
		local target_link="$user_bin_dir/$link_name"
		# Create symlink for the executable
		if [[ -L "$target_link" ]] && [[ "$(readlink -f "$target_link")" == "$(readlink -f "$bin_file")" ]]; then
			debug "Executable link already correct: $link_name"
		else
			if [[ -e "$target_link" ]]; then
				if [[ "$FORCE" == true ]]; then
					log WARNING "Overwriting existing executable: $link_name"
					if [[ "$DRY_RUN" == false ]]; then
						rm -f "$target_link"
					fi
				else
					debug "Executable already exists, skipping: $link_name"
					continue
				fi
			fi
			if [[ "$DRY_RUN" == true ]]; then
				echo "  Would link executable: $bin_name -> $link_name"
			else
				ln -sf "$bin_file" "$target_link"
				# Ensure it's executable
				chmod +x "$bin_file"
				log SUCCESS "Linked executable: $link_name"
			fi
			bin_count=$((bin_count + 1))
		fi
	done < <(find "$dotfiles_bin_dir" -maxdepth 1 -type f -print0 2>/dev/null)
	if [[ $bin_count -gt 0 ]]; then
		log INFO "Linked $bin_count executables to $user_bin_dir"
	else
		debug "No new executables to link"
	fi
}
# -----------------------------------------------------------------------------
# Claude Code configuration setup
# -----------------------------------------------------------------------------
setup_claude_config() {
	local claude_config_dir="$HOME/.claude"
	debug "Setting up Claude Code configuration..."
	# Show warning if Claude not installed
	if [[ "$HAS_CLAUDE" != true ]]; then
		log WARNING "Claude Code not installed - configs will be installed but may not be used"
	fi
	# Create .claude directory if it doesn't exist
	if [[ ! -d "$claude_config_dir" ]]; then
		debug "Creating Claude config directory: $claude_config_dir"
		if [[ "$DRY_RUN" == false ]]; then
			mkdir -p "$claude_config_dir"
		fi
	fi
	# Setup individual files
	setup_claude_file "CLAUDE.md" "$claude_config_dir/CLAUDE.md"
	setup_claude_file "settings.json" "$claude_config_dir/settings.json"
	setup_claude_file "settings.local.json" "$claude_config_dir/settings.local.json" true
	# Setup statusline.sh if exists
	if [[ -f "$DOTFILES_DIR/claude/statusline.sh" ]]; then
		setup_claude_file "statusline.sh" "$claude_config_dir/statusline.sh"
	fi
	# Setup agents directory
	if [[ -d "$DOTFILES_DIR/claude/agents" ]]; then
		local agents_dir="$claude_config_dir/agents"
		if [[ ! -d "$agents_dir" ]]; then
			debug "Creating agents directory: $agents_dir"
			if [[ "$DRY_RUN" == false ]]; then
				mkdir -p "$agents_dir"
			fi
		fi
		local agent_count=0
		local skipped_count=0
		for agent_file in "$DOTFILES_DIR/claude/agents"/*.md; do
			if [[ -f "$agent_file" ]]; then
				local agent_name="$(basename "$agent_file")"
				local agent_basename="${agent_name%.md}"
				if ! should_install_agent "$agent_basename"; then
					debug "Skipping excluded agent: $agent_name"
					skipped_count=$((skipped_count + 1))
					continue
				fi
				setup_claude_file "agents/$agent_name" "$agents_dir/$agent_name"
				agent_count=$((agent_count + 1))
			fi
		done
		if [[ $skipped_count -gt 0 ]]; then
			log INFO "Installed $agent_count agents, skipped $skipped_count"
		fi
	fi
	# Setup infrastructure directories
	setup_claude_directory "hooks" "$claude_config_dir/hooks"
	setup_claude_directory "instructions" "$claude_config_dir/instructions"
	setup_claude_directory "output-styles" "$claude_config_dir/output-styles"
	setup_claude_directory "project-templates" "$claude_config_dir/project-templates"
}
setup_claude_file() {
	local relative_path="$1"
	local target_path="$2"
	local optional="${3:-false}"
	local source_path="$DOTFILES_DIR/claude/$relative_path"
	# Check if source exists
	if [[ ! -f "$source_path" ]]; then
		if [[ "$optional" == true ]]; then
			debug "Optional Claude config file not found: $source_path (skipping)"
			return 0
		fi
		log WARNING "Claude config file not found: $source_path"
		return 1
	fi
	# Use symlinks if requested
	if [[ "$CLAUDE_SYMLINK" == true ]]; then
		# For symlinks, use the standard create_symlink function
		create_symlink "$source_path" "$target_path"
		return $?
	fi
	# Otherwise, copy the file (default behavior for beginners)
	# Check if target already exists
	if [[ -f "$target_path" ]]; then
		if [[ "$FORCE" == true ]]; then
			log WARNING "Overwriting existing Claude config: $target_path"
			if [[ "$DRY_RUN" == false ]]; then
				create_backup "$target_path"
				cp "$source_path" "$target_path"
				log SUCCESS "Updated Claude config: $relative_path"
			else
				echo "  Would overwrite: $relative_path"
			fi
		else
			debug "Claude config already exists, skipping: $relative_path"
		fi
	else
		# Copy new file
		if [[ "$DRY_RUN" == true ]]; then
			echo "  Would create: $relative_path"
		else
			cp "$source_path" "$target_path"
			log SUCCESS "Created Claude config: $relative_path"
		fi
	fi
}
setup_claude_directory() {
	local relative_path="$1"
	local target_path="$2"
	local source_path="$DOTFILES_DIR/claude/$relative_path"
	# Check if source directory exists
	if [[ ! -d "$source_path" ]]; then
		debug "Claude directory not found: $source_path (skipping)"
		return 0
	fi
	debug "Setting up Claude directory: $relative_path"
	# Create target directory if it doesn't exist
	if [[ ! -d "$target_path" ]]; then
		debug "Creating directory: $target_path"
		if [[ "$DRY_RUN" == false ]]; then
			mkdir -p "$target_path"
		fi
	fi
	# Use symlinks if requested
	if [[ "$CLAUDE_SYMLINK" == true ]]; then
		create_symlink "$source_path" "$target_path"
		return $?
	fi
	# Otherwise, copy all files in the directory
	local file_count=0
	local updated_count=0
	for source_file in "$source_path"/*; do
		if [[ -f "$source_file" ]]; then
			local file_name="$(basename "$source_file")"
			local target_file="$target_path/$file_name"
			# Check if file needs updating
			if [[ -f "$target_file" ]]; then
				if [[ "$FORCE" == true ]]; then
					if [[ "$DRY_RUN" == false ]]; then
						create_backup "$target_file"
						cp "$source_file" "$target_file"
						updated_count=$((updated_count + 1))
						debug "Updated: $relative_path/$file_name"
					else
						echo "  Would update: $relative_path/$file_name"
						updated_count=$((updated_count + 1))
					fi
				else
					debug "File exists, skipping: $relative_path/$file_name"
				fi
			else
				# Copy new file
				if [[ "$DRY_RUN" == false ]]; then
					cp "$source_file" "$target_file"
					# Make hook files executable
					if [[ "$relative_path" == "hooks" ]]; then
						chmod +x "$target_file"
					fi
					debug "Created: $relative_path/$file_name"
				else
					echo "  Would create: $relative_path/$file_name"
				fi
				file_count=$((file_count + 1))
			fi
		fi
	done
	if [[ $file_count -gt 0 || $updated_count -gt 0 ]]; then
		log SUCCESS "Setup $relative_path: $file_count new, $updated_count updated"
	fi
}
# -----------------------------------------------------------------------------
# Template generation
# -----------------------------------------------------------------------------
regenerate_templates() {
	local templates_org="$DOTFILES_DIR/emacs/templates.org"
	local templates_dir="$DOTFILES_DIR/emacs/templates"
	if [[ "$SKIP_TEMPLATES" == true ]]; then
		debug "Skipping template regeneration (--skip-templates)"
		return 0
	fi
	if [[ ! -f "$templates_org" ]]; then
		log WARNING "templates.org not found - skipping template generation"
		return 0
	fi
	if [[ "$HAS_EMACS" != true ]]; then
		log WARNING "Emacs not available - skipping template generation"
		return 0
	fi
	# Check if regeneration is needed
	local needs_regen=false
	if [[ ! -d "$templates_dir" ]]; then
		needs_regen=true
	elif [[ "$templates_org" -nt "$templates_dir" ]]; then
		needs_regen=true
	elif [[ -z "$(ls -A "$templates_dir" 2>/dev/null)" ]]; then
		needs_regen=true
	fi
	if [[ "$needs_regen" == false ]]; then
		debug "Templates are up to date"
		return 0
	fi
	log INFO "Regenerating Emacs templates from templates.org..."
	if [[ "$DRY_RUN" == true ]]; then
		echo "  Would regenerate templates from: $templates_org"
	else
		# Suppress profile warnings but show other output
		emacs -Q --batch \
			--eval "(require 'org)" \
			--eval "(org-babel-tangle-file \"$templates_org\")" \
			2>&1 | grep -v "^/etc/profile" || true
		if [[ ${PIPESTATUS[0]} -eq 0 ]]; then
			log SUCCESS "Templates regenerated successfully"
		else
			log WARNING "Template regeneration completed with warnings"
		fi
	fi
}
# -----------------------------------------------------------------------------
# Symlink creation
# -----------------------------------------------------------------------------
create_symlink() {
	local source="$1"
	local target="$2"
	local target_dir
	# Check if source exists
	if [[ ! -e "$source" ]]; then
		log ERROR "Source does not exist: $source"
		return 1
	fi
	# Create parent directory if needed
	target_dir="$(dirname "$target")"
	if [[ ! -d "$target_dir" ]]; then
		debug "Creating directory: $target_dir"
		if [[ "$DRY_RUN" == false ]]; then
			mkdir -p "$target_dir" || {
				log ERROR "Failed to create directory: $target_dir"
				return 1
			}
		fi
	fi
	# Handle existing target
	if [[ -e "$target" || -L "$target" ]]; then
		if [[ "$(readlink -f "$target")" == "$(readlink -f "$source")" ]]; then
			debug "Link already correct: $target"
			return 0
		fi
		if [[ "$FORCE" == true ]]; then
			log WARNING "Removing existing target: $target"
			if [[ "$DRY_RUN" == false ]]; then
				rm -rf "$target"
			fi
		else
			if confirm "Overwrite existing file/link: $target?"; then
				if [[ "$DRY_RUN" == false ]]; then
					rm -rf "$target"
				fi
			else
				log INFO "Skipping: $target"
				return 0
			fi
		fi
	fi
	# Create symlink
	if [[ "$DRY_RUN" == true ]]; then
		echo "  Would create: $source -> $target"
	else
		ln -sf "$source" "$target" && {
			log SUCCESS "Created: $source -> $target"
		} || {
			log ERROR "Failed to create symlink: $target"
			return 1
		}
	fi
}
copy_files() {
	local source="$1"
	local target="$2"
	local target_dir
	# Check if source exists
	if [[ ! -e "$source" ]]; then
		log ERROR "Source does not exist: $source"
		return 1
	fi
	# Create parent directory if needed
	target_dir="$(dirname "$target")"
	if [[ ! -d "$target_dir" ]]; then
		debug "Creating directory: $target_dir"
		if [[ "$DRY_RUN" == false ]]; then
			mkdir -p "$target_dir" || {
				log ERROR "Failed to create directory: $target_dir"
				return 1
			}
		fi
	fi
	# Handle existing target
	if [[ -e "$target" ]]; then
		if [[ "$FORCE" == true ]]; then
			log WARNING "Removing existing target: $target"
			if [[ "$DRY_RUN" == false ]]; then
				create_backup "$target"
				rm -rf "$target"
			fi
		else
			# For qBittorrent, offer merge option
			if [[ "$target" == *qBittorrent* ]] && [[ -d "$target" ]]; then
				if [[ "$QUICK_MODE" == true ]]; then
					# In quick mode, automatically skip existing qBittorrent config
					choice=1
					log INFO "Quick mode: Keeping existing qBittorrent config"
				else
					log INFO "qBittorrent config exists. Options:"
					echo "  1. Skip (keep existing)"
					echo "  2. Backup and replace"
					echo "  3. Merge (copy only missing files)"
					printf "  Choice [1-3]: "
					read -r choice
				fi
				case "$choice" in
				2)
					if [[ "$DRY_RUN" == false ]]; then
						create_backup "$target"
						rm -rf "$target"
					else
						echo "  Would backup and replace: $target"
					fi
					;;
				3)
					log INFO "Merging qBittorrent configs..."
					if [[ "$DRY_RUN" == false ]]; then
						cp -rn "$source"/* "$target"/ 2>/dev/null || true
						log SUCCESS "Merged missing files"
					else
						echo "  Would merge configs"
					fi
					return 0
					;;
				*)
					log INFO "Keeping existing qBittorrent config"
					return 0
					;;
				esac
			else
				log INFO "Target already exists, skipping copy: $target"
				return 0
			fi
		fi
	fi
	# Copy files/directory
	if [[ "$DRY_RUN" == true ]]; then
		echo "  Would copy: $source -> $target"
	else
		cp -r "$source" "$target" && {
			log SUCCESS "Copied: $source -> $target"
		} || {
			log ERROR "Failed to copy: $target"
			return 1
		}
	fi
}
# -----------------------------------------------------------------------------
# Credential bootstrap and preflight
# -----------------------------------------------------------------------------
declare -a ROOT_SECRET_SPECS=()
define_root_secret_specs() {
	ROOT_SECRET_SPECS=()
	case "$MACHINE" in
	mileva)
		ROOT_SECRET_SPECS=(
			"infra/meili:/root/meili.credentials"
			"infra/minio:/root/minio.credentials"
			"infra/neo4j:/root/neo4j.credentials"
			"infra/qdrant:/root/qdrant.credentials"
		)
		;;
	esac
}
password_store_entry_exists() {
	local entry="$1"
	[[ -f "$HOME/.password-store/$entry.gpg" ]] || [[ -f "$HOME/.password-store/$entry.age" ]]
}
write_first_line_from_pass() {
	local pass_entry="$1"
	local target_path="$2"
	local mode="$3"
	local temp_file
	local target_dir
	temp_file="$(mktemp "${TMPDIR:-/tmp}/dotfiles-secret.XXXXXX")"
	target_dir="$(dirname "$target_path")"
	if ! pass show "$pass_entry" 2>/dev/null | sed -n '1p' >"$temp_file"; then
		rm -f "$temp_file"
		return 1
	fi
	if [[ ! -s "$temp_file" ]]; then
		rm -f "$temp_file"
		return 1
	fi
	chmod 600 "$temp_file" 2>/dev/null || true
	if [[ "$DRY_RUN" == true ]]; then
		echo "  Would install secret: $pass_entry -> $target_path ($mode)"
		rm -f "$temp_file"
		return 0
	fi
	mkdir -p "$target_dir"
	install -m "$mode" "$temp_file" "$target_path"
	rm -f "$temp_file"
}
provision_default_ssh_identity() {
	local ssh_dir="$HOME/.ssh"
	local private_key="$ssh_dir/id_ed25519"
	local public_key="$ssh_dir/id_ed25519.pub"
	local status=0
	if [[ "$DRY_RUN" == true ]]; then
		echo "  Would ensure SSH directory exists: $ssh_dir (700)"
	else
		mkdir -p "$ssh_dir"
		chmod 700 "$ssh_dir"
	fi
	if [[ ! -f "$private_key" ]]; then
		if ! command -v pass >/dev/null 2>&1; then
			record_setup_issue SETUP_BLOCKERS \
				"SSH private key is missing at $private_key" \
				"Install 'pass' and ensure $DEFAULT_SSH_PASS_ENTRY exists, then rerun ./setup.sh."
			status=1
		elif ! password_store_entry_exists "$DEFAULT_SSH_PASS_ENTRY"; then
			record_setup_issue SETUP_BLOCKERS \
				"SSH private key is missing at $private_key" \
				"Add $DEFAULT_SSH_PASS_ENTRY to ~/.password-store, then rerun ./setup.sh."
			status=1
		elif write_first_line_from_pass "$DEFAULT_SSH_PASS_ENTRY" "$private_key" 600; then
			log SUCCESS "Installed SSH private key: $private_key"
		else
			record_setup_issue SETUP_BLOCKERS \
				"Failed to install SSH private key at $private_key" \
				"Check that $DEFAULT_SSH_PASS_ENTRY decrypts cleanly with pass, then rerun ./setup.sh."
			status=1
		fi
	fi
	if [[ ! -f "$public_key" ]]; then
		if command -v pass >/dev/null 2>&1 &&
			password_store_entry_exists "$DEFAULT_SSH_PUBLIC_PASS_ENTRY"; then
			if write_first_line_from_pass "$DEFAULT_SSH_PUBLIC_PASS_ENTRY" "$public_key" 644; then
				log SUCCESS "Installed SSH public key: $public_key"
			else
				record_setup_issue SETUP_WARNINGS \
					"Failed to install SSH public key at $public_key" \
					"Check that $DEFAULT_SSH_PUBLIC_PASS_ENTRY decrypts cleanly with pass, or regenerate it from the private key."
				status=1
			fi
		elif [[ -f "$private_key" ]] && command -v ssh-keygen >/dev/null 2>&1; then
			if [[ "$DRY_RUN" == true ]]; then
				echo "  Would derive SSH public key from $private_key"
			elif ssh-keygen -y -f "$private_key" >"$public_key"; then
				chmod 644 "$public_key"
				log SUCCESS "Derived SSH public key: $public_key"
			else
				record_setup_issue SETUP_WARNINGS \
					"Failed to derive SSH public key from $private_key" \
					"Run: ssh-keygen -y -f $private_key > $public_key"
				status=1
			fi
		fi
	fi
	return "$status"
}
provision_root_secret() {
	local pass_entry="$1"
	local target_path="$2"
	local action="Installed"
	if ! password_store_entry_exists "$pass_entry"; then
		record_setup_issue SETUP_BLOCKERS \
			"Password store entry missing for $target_path" \
			"Add $pass_entry to ~/.password-store, then rerun ./setup.sh."
		return 1
	fi
	if [[ "$DRY_RUN" == true ]]; then
		echo "  Would provision root secret: $pass_entry -> $target_path (root:root 600)"
		return 0
	fi
	if sudo test -f "$target_path"; then
		local temp_file
		temp_file="$(mktemp "${TMPDIR:-/tmp}/dotfiles-root-secret-compare.XXXXXX")"
		if ! pass show "$pass_entry" 2>/dev/null | sed -n '1p' >"$temp_file"; then
			rm -f "$temp_file"
			record_setup_issue SETUP_BLOCKERS \
				"Failed to read password store entry for $target_path" \
				"Check that $pass_entry decrypts cleanly with pass, then rerun ./setup.sh."
			return 1
		fi
		if sudo cmp -s "$temp_file" "$target_path"; then
			log INFO "Root secret already current: $target_path"
			rm -f "$temp_file"
			return 0
		fi
		rm -f "$temp_file"
		action="Updated"
	fi
	# Do not use the standard backup path here; backing up root secrets into the
	# dotfiles checkout would leak secret material into the repo worktree.
	local temp_file
	temp_file="$(mktemp "${TMPDIR:-/tmp}/dotfiles-root-secret.XXXXXX")"
	if ! pass show "$pass_entry" 2>/dev/null | sed -n '1p' >"$temp_file"; then
		rm -f "$temp_file"
		record_setup_issue SETUP_BLOCKERS \
			"Failed to read password store entry for $target_path" \
			"Check that $pass_entry decrypts cleanly with pass, then rerun ./setup.sh."
		return 1
	fi
	if [[ ! -s "$temp_file" ]]; then
		rm -f "$temp_file"
		record_setup_issue SETUP_BLOCKERS \
			"Password store entry is empty for $target_path" \
			"Put the secret on the first line of $pass_entry, then rerun ./setup.sh."
		return 1
	fi
	chmod 600 "$temp_file" 2>/dev/null || true
	sudo install -d -o root -g root -m 700 "$(dirname "$target_path")"
	if ! sudo install -o root -g root -m 600 "$temp_file" "$target_path"; then
		rm -f "$temp_file"
		record_setup_issue SETUP_BLOCKERS \
			"Failed to install root credential file at $target_path" \
			"Re-run ./setup.sh and approve the sudo prompt."
		return 1
	fi
	rm -f "$temp_file"
	log SUCCESS "$action root secret: $target_path"
}
provision_machine_root_secrets() {
	if [[ ${#ROOT_SECRET_SPECS[@]} -eq 0 ]]; then
		return 0
	fi
	if ! command -v pass >/dev/null 2>&1; then
		record_setup_issue SETUP_BLOCKERS \
			"pass is not installed but $MACHINE needs root credential bootstrap" \
			"Install pass, ensure ~/.password-store is usable, and rerun ./setup.sh."
		return 1
	fi
	if ! command -v sudo >/dev/null 2>&1; then
		record_setup_issue SETUP_BLOCKERS \
			"sudo is not available but $MACHINE needs root credential bootstrap" \
			"Install/configure sudo, then rerun ./setup.sh."
		return 1
	fi
	if [[ ! -d "$HOME/.password-store" ]]; then
		record_setup_issue SETUP_BLOCKERS \
			"Password store not found at $HOME/.password-store" \
			"Populate ~/.password-store and ~/.gnupg, then rerun ./setup.sh."
		return 1
	fi
	if [[ "$DRY_RUN" == false ]]; then
		log INFO "Authenticating for root secret provisioning..."
		if ! sudo -v; then
			record_setup_issue SETUP_BLOCKERS \
				"sudo authentication failed during root credential provisioning" \
				"Re-run ./setup.sh and complete the sudo prompt."
			return 1
		fi
	fi
	log INFO "Provisioning machine-specific root secrets..."
	local status=0
	for spec in "${ROOT_SECRET_SPECS[@]}"; do
		local pass_entry
		local target_path
		IFS=: read -r pass_entry target_path <<<"$spec"
		if ! provision_root_secret "$pass_entry" "$target_path"; then
			status=1
		fi
	done
	return "$status"
}
check_root_secret_target() {
	local pass_entry="$1"
	local target_path="$2"
	local mode
	if [[ "$DRY_RUN" == true ]]; then
		return 0
	fi
	if ! command -v sudo >/dev/null 2>&1; then
		record_setup_issue SETUP_BLOCKERS \
			"Cannot verify $target_path because sudo is unavailable" \
			"Install/configure sudo, then rerun ./setup.sh."
		return 1
	fi
	if ! sudo test -f "$target_path"; then
		record_setup_issue SETUP_BLOCKERS \
			"Root credential file missing: $target_path" \
			"Ensure $pass_entry exists in pass, then rerun ./setup.sh."
		return 1
	fi
	mode="$(sudo stat -c '%a %U:%G' "$target_path" 2>/dev/null || true)"
	if [[ "$mode" != "600 root:root" ]]; then
		record_setup_issue SETUP_BLOCKERS \
			"Unexpected ownership or mode on $target_path ($mode)" \
			"Run: sudo chown root:root $target_path && sudo chmod 600 $target_path"
		return 1
	fi
	return 0
}
check_gpg_signing_key() {
	local signing_key
	signing_key="$(git config -f "$DOTFILES_DIR/git/gitconfig" user.signingkey 2>/dev/null || true)"
	[[ -n "$signing_key" ]] || return 0
	if ! command -v gpg >/dev/null 2>&1; then
		record_setup_issue SETUP_BLOCKERS \
			"gpg is unavailable but Git commit signing is configured" \
			"Install gpg and import the secret key $signing_key."
		return 1
	fi
	if ! gpg --list-secret-keys "$signing_key" >/dev/null 2>&1; then
		record_setup_issue SETUP_BLOCKERS \
			"Git signing key $signing_key is not available in your secret keyring" \
			"Import the secret key for $signing_key, then rerun ./setup.sh."
		return 1
	fi
	return 0
}
check_ssh_identity() {
	local private_key="$HOME/.ssh/id_ed25519"
	local public_key="$HOME/.ssh/id_ed25519.pub"
	if [[ ! -f "$private_key" ]]; then
		record_setup_issue SETUP_BLOCKERS \
			"SSH private key is missing at $private_key" \
			"Ensure $DEFAULT_SSH_PASS_ENTRY exists and rerun ./setup.sh, or install the key manually."
		return 1
	fi
	if [[ "$(stat -c '%a' "$private_key" 2>/dev/null || true)" != "600" ]]; then
		record_setup_issue SETUP_WARNINGS \
			"SSH private key permissions are not 600 at $private_key" \
			"Run: chmod 600 $private_key"
	fi
	if [[ ! -f "$public_key" ]]; then
		record_setup_issue SETUP_WARNINGS \
			"SSH public key is missing at $public_key" \
			"Regenerate it with: ssh-keygen -y -f $private_key > $public_key"
	fi
	return 0
}
check_authinfo_file() {
	local authinfo_path
	authinfo_path="$(xdg_data_home)/authinfo/authinfo.gpg"
	if [[ ! -f "$authinfo_path" ]]; then
		record_setup_issue SETUP_WARNINGS \
			"Mail auth file is missing at $authinfo_path" \
			"If you use Emacs mail or git send-email, place authinfo.gpg there."
	fi
}
check_nvidia_container_runtime() {
	local runtime_path="/run/current-system/profile/bin/nvidia-container-runtime"
	if [[ "$MACHINE" != "mileva" ]]; then
		return 0
	fi
	if [[ ! -x "$runtime_path" ]]; then
		record_setup_issue SETUP_WARNINGS \
			"Docker references $runtime_path but the binary is missing" \
			"Either provide nvidia-container-runtime in the system profile or update guix/files/daemon.json to a valid runtime."
		fi
}
run_setup_preflight() {
	check_ssh_identity || true
	check_gpg_signing_key || true
	check_authinfo_file || true
	check_nvidia_container_runtime || true
	if [[ ${#ROOT_SECRET_SPECS[@]} -gt 0 ]]; then
		local spec
		for spec in "${ROOT_SECRET_SPECS[@]}"; do
			local pass_entry
			local target_path
			IFS=: read -r pass_entry target_path <<<"$spec"
			check_root_secret_target "$pass_entry" "$target_path" || true
		done
		record_setup_issue SETUP_NOTES \
			"Container services still need application-level initialization" \
			"Create your MinIO buckets, Meilisearch indexes, Neo4j schema/users, and Qdrant collections after reconfigure if you rely on them."
	fi
	if [[ "$MACHINE" == "mileva" || "$MACHINE" == "sparck" ]]; then
		record_setup_issue SETUP_NOTES \
			"Syncthing still requires one-time device pairing and folder approval" \
			"Open the Syncthing UI after login and pair the machine, then approve folders."
		record_setup_issue SETUP_NOTES \
			"GNOME visuals are installed but not selected declaratively yet" \
			"Use Tweaks to pick Yaru, Papirus, and Bibata until the dconf defaults are declared."
	fi
}
# -----------------------------------------------------------------------------
# Git hooks installation
# -----------------------------------------------------------------------------
install_git_hooks() {
	if [[ "$SKIP_HOOKS" == true ]]; then
		debug "Skipping Git hooks installation (--skip-hooks)"
		return 0
	fi
	if [[ "$IS_GIT_REPO" != true ]]; then
		debug "Not a Git repository - skipping hooks"
		return 0
	fi
	local hooks_script="$DOTFILES_DIR/git/hooks/install.sh"
	if [[ ! -f "$hooks_script" ]]; then
		log WARNING "Git hooks install script not found"
		return 0
	fi
	log INFO "Installing Git hooks..."
	if [[ "$DRY_RUN" == true ]]; then
		echo "  Would run: $hooks_script"
	else
		if "$hooks_script"; then
			log SUCCESS "Git hooks installed successfully"
		else
			log WARNING "Git hooks installation failed"
		fi
	fi
}
# -----------------------------------------------------------------------------
# Main execution
# -----------------------------------------------------------------------------
main() {
	log INFO "Dotfiles Setup Script"
	echo "====================="
	# Parse command line arguments
	parse_arguments "$@"
	# Validate conflicting options
	if [[ "$DRY_RUN" == true && "$FORCE" == true ]]; then
		die "Cannot use --dry-run and --force together"
	fi
	if [[ "$CLAUDE_UPDATE" == true && "$CLAUDE_SYMLINK" == true ]]; then
		die "Cannot use --claude-update with --claude-symlink"
	fi
	# Detect environment
	detect_environment
	define_root_secret_specs
	if [[ "$PROVISION_ROOT_SECRETS" == false ]] &&
		[[ ${#ROOT_SECRET_SPECS[@]} -gt 0 ]]; then
		PROVISION_ROOT_SECRETS=true
		AUTO_PROVISION_ROOT_SECRETS=true
	fi
	# Handle status command early
	if [[ "$SHOW_STATUS" == true ]]; then
		show_status "${STATUS_COMPONENT:-all}"
		exit 0
	fi
	# Handle update mode
	if [[ "$CLAUDE_UPDATE" == true ]]; then
		update_claude_config
		exit $?
	fi
	# Define configuration links
	define_links
	# Show what we're about to do
	if [[ "$DRY_RUN" == true ]]; then
		log INFO "Running in DRY-RUN mode - no changes will be made"
	fi
	if [[ "$GUIX_ONLY" == true ]]; then
		log INFO "Installing Guix configurations only"
	else
		log INFO "Installing all configurations"
	fi
	if [[ "$CLAUDE_SYMLINK" == true ]]; then
		log INFO "Using symlinks for Claude configs"
	fi
	if [[ -n "$CLAUDE_AGENTS" ]]; then
		log INFO "Installing only agents matching: $CLAUDE_AGENTS"
	fi
	if [[ -n "$EXCLUDE_AGENTS" ]]; then
		log INFO "Excluding agents matching: $EXCLUDE_AGENTS"
	fi
	if [[ "$PROVISION_ROOT_SECRETS" == true ]]; then
		if [[ "$AUTO_PROVISION_ROOT_SECRETS" == true ]]; then
			log INFO "Machine-specific root credential provisioning auto-enabled for $MACHINE"
		else
			log INFO "Machine-specific root credential provisioning enabled"
		fi
	fi
	echo
	# Regenerate templates if applicable
	if [[ "$GUIX_ONLY" == false ]]; then
		regenerate_templates
		echo
	fi
	# Create symlinks
	log INFO "Creating configuration symlinks..."
	local failed=0
	local created=0
	local copied=0
	local skipped=0
	local total=${#LINKS[@]}
	local current=0
	for source in "${!LINKS[@]}"; do
		current=$((current + 1))
		if [[ "$VERBOSE" == true ]] && [[ $total -gt 5 ]]; then
			printf "\r  Progress: [%d/%d]" "$current" "$total"
		fi
		if create_symlink "$source" "${LINKS[$source]}"; then
			created=$((created + 1))
		else
			failed=$((failed + 1))
		fi
	done
	[[ "$VERBOSE" == true ]] && [[ $total -gt 5 ]] && echo # Clear progress line
	# Copy files (for applications that modify their config)
	if [[ ${#COPIES[@]} -gt 0 ]]; then
		log INFO "Copying configuration files..."
		for source in "${!COPIES[@]}"; do
			if copy_files "$source" "${COPIES[$source]}"; then
				copied=$((copied + 1))
			else
				failed=$((failed + 1))
			fi
		done
	fi
	echo
	log INFO "Bootstrapping credentials..."
	clear_setup_issues
	if ! provision_default_ssh_identity; then
		log WARNING "Default SSH identity bootstrap was incomplete"
	fi
	if [[ "$PROVISION_ROOT_SECRETS" == true ]]; then
		if ! provision_machine_root_secrets; then
			log WARNING "Machine-specific root credential bootstrap was incomplete"
		fi
	fi
	echo
	# Install Git hooks if applicable
	if [[ "$GUIX_ONLY" == false ]]; then
		install_git_hooks
		echo
	fi
	run_setup_preflight
	print_setup_action_report
	local blockers_count=${#SETUP_BLOCKERS[@]}
	local warnings_count=${#SETUP_WARNINGS[@]}
	# Summary
	log INFO "Setup Summary"
	echo "============="
	echo "  Created: $created symlinks"
	[[ $copied -gt 0 ]] && echo "  Copied:  $copied directories"
	[[ $failed -gt 0 ]] && echo "  Failed:  $failed operations"
	[[ $blockers_count -gt 0 ]] && echo "  Blockers: $blockers_count"
	[[ $warnings_count -gt 0 ]] && echo "  Warnings: $warnings_count"
	[[ "$DRY_RUN" == true ]] && echo "  (DRY RUN - no actual changes made)"
	echo
	if [[ $failed -eq 0 ]] && [[ $blockers_count -eq 0 ]]; then
		log SUCCESS "Setup completed successfully!"
		# Post-setup advice
		if [[ "$GUIX_ONLY" == false ]]; then
			echo
			log INFO "Next steps:"
			echo "  1. Restart Emacs to load new configuration"
			echo "  2. Source ~/.zshrc or start a new shell"
			echo "  3. Run 'guix pull' to update channels"
			if [[ "$PROVISION_ROOT_SECRETS" == true ]]; then
				echo "  4. Re-run sudo guix system reconfigure if container secrets changed"
			fi
		fi
	else
		if [[ "$DRY_RUN" == true ]] && [[ $failed -eq 0 ]]; then
			log WARNING "Dry run found unresolved setup items"
		else
			die "Setup completed with unresolved blocking items"
		fi
	fi
}
# Cleanup on exit
cleanup() {
	rm -f "$DOTFILES_DIR/.setup_backups.tmp"
}
# Set trap for cleanup
trap cleanup EXIT
# Set trap for errors (rollback)
trap 'rollback_on_error; cleanup; exit 1' ERR
# Run main function
main "$@"
