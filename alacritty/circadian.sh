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
		'colors.primary.background="#24262b"' \
		'colors.primary.foreground="#c0c8d2"' \
		'colors.primary.dim_foreground="#9aa2ad"' \
		'colors.primary.bright_foreground="#e0e5ef"' \
		'colors.cursor.text="#24262b"' \
		'colors.cursor.cursor="#528ab7"' \
		'colors.vi_mode_cursor.text="#24262b"' \
		'colors.vi_mode_cursor.cursor="#b98a5c"' \
		'colors.search.matches.foreground="#c0c8d2"' \
		'colors.search.matches.background="#30444c"' \
		'colors.search.focused_match.foreground="#c0c8d2"' \
		'colors.search.focused_match.background="#554f43"' \
		'colors.hints.start.foreground="#24262b"' \
		'colors.hints.start.background="#98c078"' \
		'colors.hints.end.foreground="#24262b"' \
		'colors.hints.end.background="#5494c7"' \
		'colors.selection.text="CellForeground"' \
		'colors.selection.background="#364e63"' \
		'colors.normal.black="#2b2e34"' \
		'colors.normal.red="#e06c75"' \
		'colors.normal.green="#73905e"' \
		'colors.normal.yellow="#c8b07a"' \
		'colors.normal.blue="#528ab7"' \
		'colors.normal.magenta="#a05a7f"' \
		'colors.normal.cyan="#4b8b9a"' \
		'colors.normal.white="#d0d6e0"' \
		'colors.bright.black="#31343c"' \
		'colors.bright.red="#ff5f6b"' \
		'colors.bright.green="#98c078"' \
		'colors.bright.yellow="#d1a850"' \
		'colors.bright.blue="#5494c7"' \
		'colors.bright.magenta="#b06b90"' \
		'colors.bright.cyan="#52aebb"' \
		'colors.bright.white="#e0e5ef"' \
		'colors.dim.black="#1f2126"' \
		'colors.dim.red="#be737a"' \
		'colors.dim.green="#637a52"' \
		'colors.dim.yellow="#a89a78"' \
		'colors.dim.blue="#3b4751"' \
		'colors.dim.magenta="#8a6a7a"' \
		'colors.dim.cyan="#5c7f88"' \
		'colors.dim.white="#9aa2ad"' \
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
		'colors.primary.background="#fbfbfd"' \
		'colors.primary.foreground="#383a42"' \
		'colors.primary.dim_foreground="#6c6f77"' \
		'colors.primary.bright_foreground="#282c34"' \
		'colors.cursor.text="#fbfbfd"' \
		'colors.cursor.cursor="#4078f2"' \
		'colors.vi_mode_cursor.text="#fbfbfd"' \
		'colors.vi_mode_cursor.cursor="#e77727"' \
		'colors.search.matches.foreground="#383a42"' \
		'colors.search.matches.background="#d8eaf4"' \
		'colors.search.focused_match.foreground="#383a42"' \
		'colors.search.focused_match.background="#f3eada"' \
		'colors.hints.start.foreground="#fbfbfd"' \
		'colors.hints.start.background="#429b41"' \
		'colors.hints.end.foreground="#fbfbfd"' \
		'colors.hints.end.background="#1f65ff"' \
		'colors.selection.text="CellForeground"' \
		'colors.selection.background="#d6e1fb"' \
		'colors.normal.black="#282c34"' \
		'colors.normal.red="#e45649"' \
		'colors.normal.green="#50a14f"' \
		'colors.normal.yellow="#c18401"' \
		'colors.normal.blue="#4078f2"' \
		'colors.normal.magenta="#b751b6"' \
		'colors.normal.cyan="#0184bc"' \
		'colors.normal.white="#e5e5ea"' \
		'colors.bright.black="#6c6f77"' \
		'colors.bright.red="#f23726"' \
		'colors.bright.green="#429b41"' \
		'colors.bright.yellow="#ae7600"' \
		'colors.bright.blue="#1f65ff"' \
		'colors.bright.magenta="#b63db5"' \
		'colors.bright.cyan="#0076a9"' \
		'colors.bright.white="#f3f4f6"' \
		'colors.dim.black="#8a8d96"' \
		'colors.dim.red="#d3938d"' \
		'colors.dim.green="#7ca87b"' \
		'colors.dim.yellow="#c39532"' \
		'colors.dim.blue="#8aa4db"' \
		'colors.dim.magenta="#b685b5"' \
		'colors.dim.cyan="#3195bf"' \
		'colors.dim.white="#6c6f77"' \
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
		want_bg="#24262b"
		[ "$mode" = light ] && want_bg="#fbfbfd"
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
