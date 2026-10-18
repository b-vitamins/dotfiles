#!/bin/sh
# link-agent-skills.sh - Deploy this repository's agent skills to every
# supported agent runtime.
#
# Each skill under skills/ is symlinked into the user-level skills directory
# of each runtime, so editing the repository updates every agent at once:
#
#   ~/.agents/skills   Codex (and other tools that read the shared location)
#   ~/.claude/skills   Claude Code ($CLAUDE_CONFIG_DIR/skills when set)
#
# A skill is a directory under skills/ that contains SKILL.md and whose name
# does not start with "_" or ".". Symlinks into skills/ (or into the former
# codex/skills/ location) are owned by this script: stale ones are repointed
# and ones whose skill no longer exists are removed. Everything else in the
# target directories is left untouched, including Claude Code's synced/
# directory. Owned links in the old Codex location ($CODEX_HOME/skills) are
# removed, because Codex reads ~/.agents/skills.
#
# Plain POSIX sh, so the same script works on Linux and macOS.
set -eu

script_dir=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)
dotfiles_dir=$(dirname -- "$script_dir")
skills_dir=$dotfiles_dir/skills
agents_target=$HOME/.agents/skills
claude_target=${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills
legacy_dir=${CODEX_HOME:-$HOME/.codex}/skills

dry_run=false
check=false
changes=0
skill_count=0

usage() {
	cat <<EOF
Usage: $(basename -- "$0") [OPTIONS]

Symlink every skill in $skills_dir into:
  $agents_target
  $claude_target

Options:
  -n, --dry-run    Show what would change without changing anything
  -c, --check      Like --dry-run, but exit 1 if anything would change
  -h, --help       Show this help message
EOF
}

report() {
	changes=$((changes + 1))
	if [ "$dry_run" = true ]; then
		printf 'would %s: %s\n' "$1" "$2"
	else
		printf '%s: %s\n' "$1" "$2"
	fi
}

warn() {
	printf 'warning: %s\n' "$*" >&2
}

# Succeed if $1 names a deployable skill directory.
is_skill() {
	case $1 in
	_* | .*) return 1 ;;
	esac
	[ -f "$skills_dir/$1/SKILL.md" ]
}

# Succeed if $1 is a symlink this script created: one whose target lies in
# skills/ or the former codex/skills/. Links are always written with absolute
# physical paths, so a prefix test suffices and also matches dangling links.
owned_link() {
	[ -L "$1" ] || return 1
	case $(readlink "$1") in
	"$skills_dir"/* | "$dotfiles_dir"/codex/skills/*) return 0 ;;
	esac
	return 1
}

while [ $# -gt 0 ]; do
	case $1 in
	-n | --dry-run) dry_run=true ;;
	-c | --check)
		dry_run=true
		check=true
		;;
	-h | --help)
		usage
		exit 0
		;;
	*)
		usage >&2
		exit 2
		;;
	esac
	shift
done

if [ ! -d "$skills_dir" ]; then
	warn "skills directory not found: $skills_dir"
	exit 1
fi

for skill_dir in "$skills_dir"/*/; do
	skill_dir=${skill_dir%/}
	name=$(basename -- "$skill_dir")
	is_skill "$name" || continue
	skill_count=$((skill_count + 1))
	declared=$(sed -n 's/^name:[[:space:]]*//p' "$skill_dir/SKILL.md" | head -n 1 | tr -d "\"'\r")
	if [ "$declared" != "$name" ]; then
		warn "$name/SKILL.md declares name '$declared'; runtimes expect it to match the directory"
	fi
done

for target_dir in "$agents_target" "$claude_target"; do
	if [ ! -d "$target_dir" ]; then
		report "create" "$target_dir"
		[ "$dry_run" = true ] || mkdir -p -- "$target_dir"
	fi
	for skill_dir in "$skills_dir"/*/; do
		skill_dir=${skill_dir%/}
		name=$(basename -- "$skill_dir")
		is_skill "$name" || continue
		link=$target_dir/$name
		if [ -L "$link" ] && [ "$(readlink "$link")" = "$skill_dir" ]; then
			continue
		fi
		if owned_link "$link"; then
			report "repoint" "$link -> $skill_dir"
			[ "$dry_run" = true ] || ln -sfn "$skill_dir" "$link"
		elif [ -e "$link" ] || [ -L "$link" ]; then
			warn "$link exists and is not managed by this script; leaving it alone"
		else
			report "link" "$link -> $skill_dir"
			[ "$dry_run" = true ] || ln -s "$skill_dir" "$link"
		fi
	done
done

for dir in "$agents_target" "$claude_target" "$legacy_dir"; do
	[ -d "$dir" ] || continue
	for link in "$dir"/*; do
		owned_link "$link" || continue
		if [ "$dir" = "$legacy_dir" ]; then
			report "remove legacy link" "$link"
		elif ! is_skill "$(basename -- "$link")"; then
			report "remove stale link" "$link"
		else
			continue
		fi
		[ "$dry_run" = true ] || rm -f -- "$link"
	done
done

if [ "$changes" -eq 0 ]; then
	printf '%d skills: up to date\n' "$skill_count"
elif [ "$dry_run" = true ]; then
	printf '%d skills: %d change(s) pending; run %s to apply\n' \
		"$skill_count" "$changes" "$0"
else
	printf '%d skills: %d change(s) applied\n' "$skill_count" "$changes"
fi

if [ "$check" = true ] && [ "$changes" -gt 0 ]; then
	exit 1
fi
exit 0
