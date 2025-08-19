#!/usr/bin/env bash

set -uo pipefail

# Script to provision .stignore files for Syncthing-managed directories
# Handles both project directories and home directories

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEMPLATES_DIR="${SCRIPT_DIR}/../syncthing/templates"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

usage() {
	cat <<EOF
Usage: $(basename "$0") [OPTIONS] [MODE]

Provisions .stignore files for directories based on their type.

Modes:
  projects [DIR]    Provision project directories (default: ~/projects)
  home              Provision home directories (.gnupg, .password-store, etc.)
  all               Provision both projects and home directories
  [DIR]            Provision a specific directory

Options:
  -h, --help       Show this help message
  -f, --force      Overwrite existing .stignore files
  -d, --dry-run    Show what would be done without making changes
  -l, --list       List available templates

Examples:
  $(basename "$0")                        # Provision ~/projects
  $(basename "$0") projects                # Provision ~/projects
  $(basename "$0") home                    # Provision home directories
  $(basename "$0") all                     # Provision everything
  $(basename "$0") ~/workspace             # Provision specific directory
  $(basename "$0") --force all            # Force overwrite everything
  $(basename "$0") --list                  # Show available templates
EOF
}

# Parse arguments - collect flags and modes separately
FORCE=false
DRY_RUN=false
LIST_TEMPLATES=false
MODE=""
TARGET_DIR=""
POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
	case $1 in
	-h | --help)
		usage
		exit 0
		;;
	-f | --force)
		FORCE=true
		shift
		;;
	-d | --dry-run)
		DRY_RUN=true
		shift
		;;
	-l | --list)
		LIST_TEMPLATES=true
		shift
		;;
	-*)
		echo -e "${RED}Error: Unknown option: $1${NC}"
		echo ""
		usage
		exit 1
		;;
	*)
		POSITIONAL_ARGS+=("$1")
		shift
		;;
	esac
done

# Process positional arguments (only if not just listing templates)
if [ "$LIST_TEMPLATES" = false ]; then
	for arg in "${POSITIONAL_ARGS[@]:-}"; do
		case $arg in
		projects | home | all)
			if [[ -n "$MODE" ]]; then
				# If mode already set, this might be a directory
				if [[ "$MODE" == "projects" ]]; then
					TARGET_DIR="$arg"
				else
					echo -e "${RED}Error: Multiple modes specified${NC}"
					exit 1
				fi
			else
				MODE="$arg"
			fi
			;;
		*)
			# Check if it's a directory or could be one
			if [[ -d "$arg" || "$DRY_RUN" == true || "$arg" == ~/* || "$arg" == /* || "$arg" == ./* ]]; then
				if [[ "$MODE" == "projects" && -z "$TARGET_DIR" ]]; then
					TARGET_DIR="$arg"
				elif [[ -z "$MODE" ]]; then
					MODE="dir"
					TARGET_DIR="$arg"
				else
					echo -e "${RED}Error: Multiple directories specified${NC}"
					exit 1
				fi
			else
				echo -e "${RED}Error: Unknown argument: $arg${NC}"
				echo "Not a valid mode or directory"
				echo ""
				usage
				exit 1
			fi
			;;
		esac
	done
fi

# Default mode
if [[ -z "$MODE" ]]; then
	MODE="projects"
fi

# Verify templates directory exists
if [ ! -d "$TEMPLATES_DIR" ]; then
	echo -e "${RED}Error: Templates directory not found at $TEMPLATES_DIR${NC}"
	echo "Please ensure you're running this from the dotfiles repository"
	exit 1
fi

# List templates function
list_templates() {
	echo -e "${BLUE}Available templates in:${NC} $TEMPLATES_DIR"
	echo ""
	echo -e "${GREEN}Project templates:${NC}"
	for template in "$TEMPLATES_DIR"/stignore-*; do
		if [ -f "$template" ]; then
			name=$(basename "$template" | sed 's/stignore-//')
			case $name in
			base | python | rust | js | cpp | latex | guix)
				printf "  %-15s" "$name"
				# Show first meaningful comment from template
				first_comment=$(grep "^//" "$template" | head -1 | sed 's|^// *||')
				[ -n "$first_comment" ] && echo "- $first_comment" || echo ""
				;;
			esac
		fi
	done
	echo ""
	echo -e "${GREEN}Special templates:${NC}"
	for template in "$TEMPLATES_DIR"/stignore-*; do
		if [ -f "$template" ]; then
			name=$(basename "$template" | sed 's/stignore-//')
			case $name in
			gnupg | password-store | documents | media)
				printf "  %-15s" "$name"
				# Show first meaningful comment from template
				first_comment=$(grep "^//" "$template" | head -1 | sed 's|^// *||')
				[ -n "$first_comment" ] && echo "- $first_comment" || echo ""
				;;
			esac
		fi
	done
}

# Function to detect project type
detect_project_type() {
	local dir="$1"
	local types=()

	[ -f "${dir}/pyproject.toml" ] || [ -f "${dir}/setup.py" ] || [ -f "${dir}/requirements.txt" ] && types+=("python")
	[ -f "${dir}/Cargo.toml" ] && types+=("rust")
	[ -f "${dir}/package.json" ] && types+=("js")
	[ -f "${dir}/CMakeLists.txt" ] && types+=("cpp")
	[ -f "${dir}/manifest.scm" ] && types+=("guix")
	# Check for LaTeX files
	if find "${dir}" -maxdepth 1 -name "*.tex" -print -quit 2>/dev/null | grep -q .; then
		types+=("latex")
	fi

	if [ ${#types[@]} -gt 0 ]; then
		printf '%s\n' "${types[@]}"
	fi
}

# Function to safely write to file with permission check
safe_write() {
	local file="$1"
	local content_file="$2"

	# Try to create/write the file
	if cat "$content_file" >"$file" 2>/dev/null; then
		return 0
	else
		echo -e "${RED}  Error: Cannot write to $file (permission denied or disk full)${NC}"
		return 1
	fi
}

# Function to create .stignore for a project directory
create_project_stignore() {
	local dir="$1"
	local name
	name=$(basename "$dir")
	local stignore_file="${dir}/.stignore"

	# Check if directory is writable
	if [ "$DRY_RUN" = false ] && [ ! -w "$dir" ]; then
		echo -e "${RED}Error:${NC} Cannot write to $name (permission denied)"
		return 1
	fi

	# Check if .stignore exists
	if [ -f "$stignore_file" ] && [ "$FORCE" = false ]; then
		echo -e "${YELLOW}Skipping${NC} $name - .stignore already exists"
		return 0
	fi

	# Detect project types
	local types
	mapfile -t types < <(detect_project_type "$dir")

	if [ "$DRY_RUN" = true ]; then
		if [ ${#types[@]} -gt 0 ]; then
			echo -e "${GREEN}Would create${NC} .stignore for $name with: base + ${types[*]}"
		else
			echo -e "${GREEN}Would create${NC} .stignore for $name with: base"
		fi
		return 0
	fi

	echo -e "${GREEN}Creating${NC} .stignore for $name"

	# Create temporary file
	local temp_file
	temp_file=$(mktemp)

	# Start with base template
	cat "${TEMPLATES_DIR}/stignore-base" >"$temp_file"
	echo "" >>"$temp_file"

	# Add language-specific patterns
	for type in "${types[@]}"; do
		template_file="${TEMPLATES_DIR}/stignore-${type}"
		if [ -f "$template_file" ]; then
			echo "  Adding ${type} patterns..."
			cat "$template_file" >>"$temp_file"
			echo "" >>"$temp_file"
		fi
	done

	# Add special Guix patterns if needed
	if [[ " ${types[*]} " =~ " guix " ]]; then
		cat >>"$temp_file" <<'EOF'
// Guix build artifacts
*.go
*.drv
.guix-profile/
.guix-home/
EOF
	fi

	# Write the file
	if safe_write "$stignore_file" "$temp_file"; then
		echo -e "  ${GREEN}✓${NC} Successfully created .stignore"
	else
		echo -e "  ${RED}✗${NC} Failed to create .stignore"
	fi

	# Clean up temp file
	rm -f "$temp_file"
}

# Function to provision a specific home directory
provision_home_directory() {
	local dir="$1"
	local template="$2"
	local name
	name=$(basename "$dir")

	# Expand tilde in path
	dir="${dir/#\~/$HOME}"

	# Check if directory exists
	if [ ! -d "$dir" ]; then
		if [ "$DRY_RUN" = true ]; then
			echo -e "${YELLOW}Would skip${NC} $name - directory does not exist"
		else
			echo -e "${YELLOW}Skipping${NC} $name - directory does not exist"
		fi
		return
	fi

	local stignore_file="${dir}/.stignore"

	# Check if directory is writable
	if [ "$DRY_RUN" = false ] && [ ! -w "$dir" ]; then
		echo -e "${RED}Error:${NC} Cannot write to $name (permission denied)"
		return 1
	fi

	# Check if .stignore exists
	if [ -f "$stignore_file" ] && [ "$FORCE" = false ]; then
		echo -e "${YELLOW}Skipping${NC} $name - .stignore already exists"
		return
	fi

	# Check if template exists
	if [ ! -f "${TEMPLATES_DIR}/${template}" ]; then
		echo -e "${RED}Error:${NC} Template $template not found"
		return 1
	fi

	if [ "$DRY_RUN" = true ]; then
		echo -e "${GREEN}Would create${NC} .stignore for $name using template: ${template#stignore-}"
		return
	fi

	echo -e "${GREEN}Creating${NC} .stignore for $name"

	# Copy template to destination
	if cp "${TEMPLATES_DIR}/${template}" "$stignore_file" 2>/dev/null; then
		echo -e "  ${GREEN}✓${NC} Successfully created .stignore"
	else
		echo -e "  ${RED}✗${NC} Failed to create .stignore (permission denied)"
	fi
}

# Provision project directories
provision_projects() {
	local target="${1:-$HOME/projects}"

	# Expand tilde in path
	target="${target/#\~/$HOME}"

	echo -e "${BLUE}Provisioning .stignore files in:${NC} ${target}"

	if [ ! -d "$target" ] && [ "$DRY_RUN" = false ]; then
		echo -e "${RED}Error: Target directory $target does not exist${NC}"
		return 1
	fi

	if [ "$DRY_RUN" = false ] && [ ! -r "$target" ]; then
		echo -e "${RED}Error: Cannot read directory $target (permission denied)${NC}"
		return 1
	fi

	local count=0
	local success=0
	local failed=0

	# Check if directory has subdirectories
	local has_subdirs=false
	for dir in "${target}"/*/; do
		if [ -d "$dir" ]; then
			has_subdirs=true
			break
		fi
	done

	if [ "$has_subdirs" = false ]; then
		echo -e "${YELLOW}No subdirectories found in $target${NC}"
		return 0
	fi

	for dir in "${target}"/*/; do
		if [ -d "$dir" ]; then
			create_project_stignore "$dir"
			result=$?
			((count++))
			if [ $result -eq 0 ]; then
				((success++))
			else
				((failed++))
			fi
		fi
	done

	echo ""
	echo -e "${BLUE}Summary:${NC} Processed $count directories"
	if [ "$DRY_RUN" = false ]; then
		[ $success -gt 0 ] && echo -e "  ${GREEN}✓${NC} $success successful"
		[ $failed -gt 0 ] && echo -e "  ${RED}✗${NC} $failed failed"
	fi
}

# Provision home directories
provision_home() {
	echo -e "${BLUE}Provisioning .stignore files for home directories${NC}"
	echo ""

	local dirs_processed=0

	# Special directories
	provision_home_directory "$HOME/.gnupg" "stignore-gnupg" && ((dirs_processed++))
	provision_home_directory "$HOME/.password-store" "stignore-password-store" && ((dirs_processed++))

	# Document directories
	provision_home_directory "$HOME/documents" "stignore-documents" && ((dirs_processed++))

	# Media directories
	provision_home_directory "$HOME/music" "stignore-media" && ((dirs_processed++))
	provision_home_directory "$HOME/pictures" "stignore-media" && ((dirs_processed++))
	provision_home_directory "$HOME/videos" "stignore-media" && ((dirs_processed++))

	# General sync directory
	provision_home_directory "$HOME/sync" "stignore-base" && ((dirs_processed++))

	# Note about downloads
	if [ -d "$HOME/downloads" ]; then
		echo ""
		echo -e "${YELLOW}Note:${NC} ~/downloads typically shouldn't be synced (too dynamic)"
	fi

	echo ""
	echo -e "${BLUE}Summary:${NC} Attempted to provision ${dirs_processed} home directories"
}

# Main execution
if [ "$LIST_TEMPLATES" = true ]; then
	list_templates
	exit 0
fi

# Show what we're doing
if [ "$DRY_RUN" = true ]; then
	echo -e "${YELLOW}DRY RUN MODE - No files will be modified${NC}"
fi

if [ "$FORCE" = true ] && [ "$DRY_RUN" = false ]; then
	echo -e "${YELLOW}FORCE MODE - Existing .stignore files will be overwritten${NC}"
fi

echo -e "${BLUE}Templates from:${NC} ${TEMPLATES_DIR}"
echo ""

case "$MODE" in
projects)
	provision_projects "$TARGET_DIR"
	;;
home)
	provision_home
	;;
all)
	provision_projects
	echo ""
	provision_home
	;;
dir)
	if [ -n "$TARGET_DIR" ]; then
		provision_projects "$TARGET_DIR"
	else
		echo -e "${RED}Error: No directory specified${NC}"
		exit 1
	fi
	;;
*)
	echo -e "${RED}Error: Unknown mode: $MODE${NC}"
	exit 1
	;;
esac

echo ""
if [ "$DRY_RUN" = true ]; then
	echo -e "${YELLOW}This was a dry run. No files were modified.${NC}"
	echo "Remove --dry-run flag to apply changes."
fi
