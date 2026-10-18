#!/bin/sh

# gnome-session-shepherd can start MediaKeys before the compositor has
# exported its display environment. Give its child applications a display.
set -eu

if [ "$#" -eq 0 ] || [ ! -x "$1" ]; then
    echo "Usage: $0 /path/to/gsd-media-keys [arguments...]" >&2
    exit 2
fi

if [ -z "${WAYLAND_DISPLAY:-}" ] && [ -z "${WAYLAND_SOCKET:-}" ] &&
   [ -z "${DISPLAY:-}" ] &&
   [ "${XDG_SESSION_TYPE:-}" = wayland ]; then
    if [ -z "${XDG_RUNTIME_DIR:-}" ]; then
        echo "Error: XDG_RUNTIME_DIR is required to locate the Wayland display." >&2
        exit 1
    fi

    attempts=0
    while [ "$attempts" -lt 30 ]; do
        display_socket=
        for candidate in "$XDG_RUNTIME_DIR"/wayland-*; do
            [ -S "$candidate" ] || continue
            if [ -n "$display_socket" ]; then
                echo "Error: Multiple Wayland displays; set WAYLAND_DISPLAY explicitly." >&2
                exit 1
            fi
            display_socket=$candidate
        done

        if [ -n "$display_socket" ]; then
            WAYLAND_DISPLAY=${display_socket##*/}
            export WAYLAND_DISPLAY
            break
        fi
        attempts=$((attempts + 1))
        "${GNOME_MEDIA_KEYS_SLEEP:-sleep}" 1
    done

    if [ -z "${WAYLAND_DISPLAY:-}" ]; then
        echo "Error: No Wayland display appeared in $XDG_RUNTIME_DIR within 30 seconds." >&2
        exit 1
    fi
fi

exec "$@"
