#!/bin/sh
set -eu

	usage() {
		cat <<'EOF'
	circadian.sh — time-based light/dark theme switching for Alacritty.

	Usage:
	  circadian.sh [--apply]
	  circadian.sh --print
	  circadian.sh --mode (light|dark)

Defaults:
  Light: 07:00–19:00 local time

Optional config:
  $XDG_CONFIG_HOME/circadian/alacritty.env (shell env file)
    CIRCADIAN_LIGHT_START="07:00"
    CIRCADIAN_DARK_START="19:00"
EOF
	}

die() { printf '%s\n' "$*" >&2; exit 2; }

to_minutes() {
	# Converts HH:MM -> minutes since midnight.
	case "${1-}" in
		??:??) ;;
		*) die "invalid time: ${1-} (expected HH:MM)" ;;
	esac
	h=${1%:*}
	m=${1#*:}
	# shellcheck disable=SC2004
	echo $((10#$h * 60 + 10#$m))
}

circadian_mode_from_time() {
	now_min=$(to_minutes "$1")
	light_min=$(to_minutes "$2")
	dark_min=$(to_minutes "$3")

	if [ "$light_min" -lt "$dark_min" ]; then
		if [ "$now_min" -ge "$light_min" ] && [ "$now_min" -lt "$dark_min" ]; then
			echo light
		else
			echo dark
		fi
	else
		# Wraparound (e.g. light 19:00 -> 07:00).
		if [ "$now_min" -ge "$light_min" ] || [ "$now_min" -lt "$dark_min" ]; then
			echo light
		else
			echo dark
		fi
	fi
}

apply_alacritty_runtime_theme_dark() {
	command -v alacritty >/dev/null 2>&1 || return 0
	alacritty msg get-config >/dev/null 2>&1 || return 0

	alacritty msg config -w -1 \
		'window.opacity=0.97' \
		'window.blur=true' \
		'window.decorations_theme_variant="Dark"' \
		'colors.draw_bold_text_with_bright_colors=false' \
		'colors.transparent_background_colors=true' \
		'colors.primary.background="#0e1419"' \
		'colors.primary.foreground="#d8dee9"' \
		'colors.primary.dim_foreground="#a5abb6"' \
		'colors.primary.bright_foreground="#eceff4"' \
		'colors.cursor.text="#0e1419"' \
		'colors.cursor.cursor="#81a1c1"' \
		'colors.vi_mode_cursor.text="#0e1419"' \
		'colors.vi_mode_cursor.cursor="#ffca28"' \
		'colors.search.matches.foreground="#0e1419"' \
		'colors.search.matches.background="#81a1c1"' \
		'colors.search.focused_match.foreground="#0e1419"' \
		'colors.search.focused_match.background="#ffca28"' \
		'colors.hints.start.foreground="#0e1419"' \
		'colors.hints.start.background="#66bb6a"' \
		'colors.hints.end.foreground="#0e1419"' \
		'colors.hints.end.background="#81a1c1"' \
		'colors.selection.text="CellForeground"' \
		'colors.selection.background="#2e3440"' \
		'colors.normal.black="#0e1419"' \
		'colors.normal.red="#ef5350"' \
		'colors.normal.green="#66bb6a"' \
		'colors.normal.yellow="#ffca28"' \
		'colors.normal.blue="#42a5f5"' \
		'colors.normal.magenta="#ab47bc"' \
		'colors.normal.cyan="#26c6da"' \
		'colors.normal.white="#cfd8dc"' \
		'colors.bright.black="#546e7a"' \
		'colors.bright.red="#ff6b68"' \
		'colors.bright.green="#81c784"' \
		'colors.bright.yellow="#ffd54f"' \
		'colors.bright.blue="#64b5f6"' \
		'colors.bright.magenta="#ba68c8"' \
		'colors.bright.cyan="#4dd0e1"' \
		'colors.bright.white="#eceff1"' \
		'colors.dim.black="#263238"' \
		'colors.dim.red="#d32f2f"' \
		'colors.dim.green="#388e3c"' \
		'colors.dim.yellow="#f57c00"' \
		'colors.dim.blue="#1976d2"' \
		'colors.dim.magenta="#7b1fa2"' \
		'colors.dim.cyan="#0097a7"' \
		'colors.dim.white="#90a4ae"' \
		>/dev/null 2>&1 || true
}

apply_alacritty_runtime_theme_light() {
	command -v alacritty >/dev/null 2>&1 || return 0
	alacritty msg get-config >/dev/null 2>&1 || return 0

	alacritty msg config -w -1 \
		'window.opacity=1.0' \
		'window.blur=false' \
		'window.decorations_theme_variant="Light"' \
		'colors.draw_bold_text_with_bright_colors=false' \
		'colors.transparent_background_colors=true' \
		'colors.primary.background="#fbfbfc"' \
		'colors.primary.foreground="#1f2328"' \
		'colors.primary.dim_foreground="#57606a"' \
		'colors.primary.bright_foreground="#0b0f14"' \
		'colors.cursor.text="#fbfbfc"' \
		'colors.cursor.cursor="#0969da"' \
		'colors.vi_mode_cursor.text="#fbfbfc"' \
		'colors.vi_mode_cursor.cursor="#9a6700"' \
		'colors.search.matches.foreground="#1f2328"' \
		'colors.search.matches.background="#a5d6ff"' \
		'colors.search.focused_match.foreground="#1f2328"' \
		'colors.search.focused_match.background="#ffd8a8"' \
		'colors.hints.start.foreground="#1f2328"' \
		'colors.hints.start.background="#b7eb8f"' \
		'colors.hints.end.foreground="#1f2328"' \
		'colors.hints.end.background="#a5d6ff"' \
		'colors.selection.text="CellForeground"' \
		'colors.selection.background="#e7effb"' \
		'colors.normal.black="#24292f"' \
		'colors.normal.red="#cf222e"' \
		'colors.normal.green="#116329"' \
		'colors.normal.yellow="#9a6700"' \
		'colors.normal.blue="#0969da"' \
		'colors.normal.magenta="#8250df"' \
		'colors.normal.cyan="#1b7c83"' \
		'colors.normal.white="#57606a"' \
		'colors.bright.black="#6e7781"' \
		'colors.bright.red="#ff4d4f"' \
		'colors.bright.green="#1a7f37"' \
		'colors.bright.yellow="#bf8700"' \
		'colors.bright.blue="#218bff"' \
		'colors.bright.magenta="#a475f9"' \
		'colors.bright.cyan="#3192aa"' \
		'colors.bright.white="#1f2328"' \
		'colors.dim.black="#8c959f"' \
		'colors.dim.red="#a40e26"' \
		'colors.dim.green="#2da44e"' \
		'colors.dim.yellow="#7d4e00"' \
		'colors.dim.blue="#0550ae"' \
		'colors.dim.magenta="#6639ba"' \
		'colors.dim.cyan="#0f766e"' \
		'colors.dim.white="#6e7781"' \
		>/dev/null 2>&1 || true
}

main() {
	mode=""
	do_print=false
	do_apply=true

	while [ "$#" -gt 0 ]; do
		case "$1" in
			-h|--help) usage; exit 0 ;;
			--print) do_print=true; do_apply=false ;;
			--apply) do_apply=true ;;
			--mode)
				[ "$#" -ge 2 ] || die "--mode requires an argument"
				mode=$2
				shift
				;;
			*)
				die "unknown argument: $1"
				;;
		esac
		shift
	done

	config_home=${XDG_CONFIG_HOME:-"$HOME/.config"}
	state_home=${XDG_STATE_HOME:-"$HOME/.local/state"}
	cfg_dir="$config_home/circadian"
	cfg_file="$cfg_dir/alacritty.env"

	light_start=${CIRCADIAN_LIGHT_START:-"07:00"}
	dark_start=${CIRCADIAN_DARK_START:-"19:00"}
	if [ -r "$cfg_file" ]; then
		# shellcheck disable=SC1090
		. "$cfg_file"
	fi
	light_start=${CIRCADIAN_LIGHT_START:-$light_start}
	dark_start=${CIRCADIAN_DARK_START:-$dark_start}

	alacritty_dir="$config_home/alacritty"
	theme_file="$alacritty_dir/theme.toml"
	mode_file="$state_home/circadian/mode"

	if [ -z "$mode" ]; then
		now=$(date +%H:%M)
		mode=$(circadian_mode_from_time "$now" "$light_start" "$dark_start")
	fi

	case "$mode" in
		light|dark) ;;
		*) die "invalid mode: $mode (expected light|dark)" ;;
	esac

	if [ "$do_print" = true ]; then
		printf '%s\n' "$mode"
		exit 0
	fi

	[ "$do_apply" = true ] || exit 0

	mkdir -p "$alacritty_dir" "$(dirname "$mode_file")" "$cfg_dir"

	current_mode=""
	if [ -r "$mode_file" ]; then
		current_mode=$(tr -d '\r\n' <"$mode_file" 2>/dev/null || true)
	fi

	mode_changed=false
	if [ "$current_mode" != "$mode" ]; then
		mode_changed=true
	fi

	theme_file_updated=false
	if [ "$mode_changed" = true ] || [ ! -f "$theme_file" ]; then
		tmp_theme=$(mktemp)
		cat >"$tmp_theme" <<EOF
[general]
import = ["themes/bv-$mode.toml"]
EOF
		mv "$tmp_theme" "$theme_file"
		printf '%s\n' "$mode" >"$mode_file"
		theme_file_updated=true
	fi

	# Apply immediately to running Alacritty windows (without resetting other runtime changes).
	# Avoid spamming IPC unless the active palette is actually different.
	if [ "$theme_file_updated" = true ]; then
		want_bg="#0e1419"
		[ "$mode" = light ] && want_bg="#fbfbfc"
		if command -v alacritty >/dev/null 2>&1 && command -v jq >/dev/null 2>&1; then
			have_bg=$(alacritty msg get-config 2>/dev/null | jq -r '.colors.primary.background // empty' 2>/dev/null || true)
			if [ "$have_bg" != "$want_bg" ]; then
				case "$mode" in
					light) apply_alacritty_runtime_theme_light ;;
					dark) apply_alacritty_runtime_theme_dark ;;
				esac
			fi
		elif command -v alacritty >/dev/null 2>&1; then
			case "$mode" in
				light) apply_alacritty_runtime_theme_light ;;
				dark) apply_alacritty_runtime_theme_dark ;;
			esac
		fi
	fi
}

main "$@"
