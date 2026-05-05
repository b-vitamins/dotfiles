#!/usr/bin/env bash
set -Eeuo pipefail

OVPN_PASS_PREFIX=${PROTONVPN_OVPN_PASS_PREFIX:-infra/protonvpn/ovpn}
CREDENTIAL_PASS_ENTRY=${PROTONVPN_CREDENTIAL_PASS_ENTRY:-infra/protonvpn/ikev2}

DRY_RUN=false
REPLACE=false
SET_SECRETS=true
LIST_ONLY=false

declare -a SELECTORS=()
declare -a SELECTED_PROFILES=()

SCRIPT_NAME=$(basename "$0")
readonly SCRIPT_NAME

DEFAULT_PROFILES=$(
	cat <<'EOF'
ca-428.tcp|ProtonVPN CA-428 Canada|CA|Canada
ch-416.tcp|ProtonVPN CH-416 Switzerland|CH|Switzerland
is-30.tcp|ProtonVPN IS-30 Iceland|IS|Iceland
my-42.tcp|ProtonVPN MY-42 Malaysia|MY|Malaysia
nl-547.tcp|ProtonVPN NL-547 Netherlands|NL|Netherlands
ro-26.tcp|ProtonVPN RO-26 Romania|RO|Romania
sg-98.tcp|ProtonVPN SG-98 Singapore|SG|Singapore
EOF
)
readonly DEFAULT_PROFILES

usage() {
	cat <<EOF
Usage: $SCRIPT_NAME [OPTIONS] [COUNTRY_OR_CODE ...]

Import a curated ProtonVPN OpenVPN country set from pass into NetworkManager.
With no country arguments, all default profiles are imported.

Defaults:
  OVPN pass prefix:       $OVPN_PASS_PREFIX
  Credential pass entry:  $CREDENTIAL_PASS_ENTRY

Options:
  -h, --help              Show this help text
  -l, --list              List the built-in country/profile set
  -n, --dry-run           Show what would be changed
  -r, --replace           Delete and re-import matching existing profiles
      --no-secrets        Import profiles but do not store VPN credentials
      --ovpn-prefix PATH  pass prefix containing .tcp OpenVPN configs
      --credentials PATH  pass entry containing OpenVPN password/user

Country selectors may be country names, ISO-like codes, server names, or
profile substrings, for example:
  $SCRIPT_NAME
  $SCRIPT_NAME Netherlands Switzerland Singapore
  $SCRIPT_NAME nl ch sg
  $SCRIPT_NAME --replace ro-26

The default credential parser accepts either:
  password on line 1 and username on line 2, or
  labeled lines like "password: ..." and "username: ..."
EOF
}

die() {
	printf 'error: %s\n' "$*" >&2
	exit 1
}

warn() {
	printf 'warning: %s\n' "$*" >&2
}

info() {
	printf '%s\n' "$*"
}

need_cmd() {
	command -v "$1" >/dev/null 2>&1 || die "$1 not found in PATH"
}

lower() {
	printf '%s' "$1" | tr '[:upper:]' '[:lower:]'
}

connection_exists() {
	nmcli connection show "$1" >/dev/null 2>&1
}

sanitize_log() {
	sed -E \
		-e 's/(password=)[^,[:space:]]+/\1REDACTED/g' \
		-e 's/(vpn\.secrets[[:space:]]+password=)[^,[:space:]]+/\1REDACTED/g' \
		"$1" >&2
}

append_selected_once() {
	local row=$1
	local existing

	for existing in "${SELECTED_PROFILES[@]:-}"; do
		[[ "$existing" == "$row" ]] && return 0
	done

	SELECTED_PROFILES+=("$row")
}

profile_matches_selector() {
	local row=$1
	local selector=$2
	local entry profile code country base
	local s entry_l profile_l code_l country_l base_l

	IFS='|' read -r entry profile code country <<<"$row"
	base=${entry%.tcp}

	s=$(lower "$selector")
	entry_l=$(lower "$entry")
	profile_l=$(lower "$profile")
	code_l=$(lower "$code")
	country_l=$(lower "$country")
	base_l=$(lower "$base")

	[[ "$s" == "$entry_l" ||
		"$s" == "$base_l" ||
		"$s" == "$code_l" ||
		"$s" == "$country_l" ||
		"$profile_l" == *"$s"* ]]
}

build_selection() {
	local row selector found

	if ((${#SELECTORS[@]} == 0)); then
		while IFS= read -r row; do
			[[ -n "$row" ]] && append_selected_once "$row"
		done <<<"$DEFAULT_PROFILES"
		return 0
	fi

	for selector in "${SELECTORS[@]}"; do
		found=false
		while IFS= read -r row; do
			[[ -z "$row" ]] && continue
			if profile_matches_selector "$row" "$selector"; then
				append_selected_once "$row"
				found=true
			fi
		done <<<"$DEFAULT_PROFILES"

		"$found" || die "unknown country/profile selector: $selector"
	done
}

list_profiles() {
	local row entry profile code country

	printf '%-4s  %-12s  %-12s  %s\n' Code Country Entry "NetworkManager profile"
	while IFS= read -r row; do
		[[ -z "$row" ]] && continue
		IFS='|' read -r entry profile code country <<<"$row"
		printf '%-4s  %-12s  %-12s  %s\n' "$code" "$country" "$entry" "$profile"
	done <<<"$DEFAULT_PROFILES"
}

parse_args() {
	while (($# > 0)); do
		case "$1" in
		-h | --help)
			usage
			exit 0
			;;
		-l | --list)
			LIST_ONLY=true
			shift
			;;
		-n | --dry-run)
			DRY_RUN=true
			shift
			;;
		-r | --replace)
			REPLACE=true
			shift
			;;
		--no-secrets)
			SET_SECRETS=false
			shift
			;;
		--ovpn-prefix)
			(($# >= 2)) || die "--ovpn-prefix needs a pass path"
			OVPN_PASS_PREFIX=$2
			shift 2
			;;
		--credentials)
			(($# >= 2)) || die "--credentials needs a pass path"
			CREDENTIAL_PASS_ENTRY=$2
			shift 2
			;;
		--)
			shift
			while (($# > 0)); do
				SELECTORS+=("$1")
				shift
			done
			;;
		-*)
			die "unknown option: $1"
			;;
		*)
			SELECTORS+=("$1")
			shift
			;;
		esac
	done
}

load_credentials() {
	local line key value first second
	local -a positional=()

	VPN_USER=
	VPN_PASS=

	while IFS= read -r line || [[ -n "$line" ]]; do
		line=${line%$'\r'}
		[[ -z "${line//[[:space:]]/}" ]] && continue

		if [[ "$line" =~ ^[[:space:]]*([^:]+)[[:space:]]*:[[:space:]]*(.*)$ ]]; then
			key=$(lower "${BASH_REMATCH[1]}")
			value=${BASH_REMATCH[2]}
			case "$key" in
			username | user)
				VPN_USER=$value
				;;
			password | pass)
				VPN_PASS=$value
				;;
			*)
				positional+=("$line")
				;;
			esac
		else
			positional+=("$line")
		fi
	done < <(pass show "$CREDENTIAL_PASS_ENTRY")

	if [[ -z "$VPN_USER" || -z "$VPN_PASS" ]]; then
		((${#positional[@]} >= 2)) ||
			die "credential entry $CREDENTIAL_PASS_ENTRY must contain at least password and username"

		first=${positional[0]}
		second=${positional[1]}

		if [[ "$first" == *+* && "$second" != *+* ]]; then
			VPN_USER=$first
			VPN_PASS=$second
		else
			VPN_PASS=$first
			VPN_USER=$second
		fi
	fi

	[[ -n "$VPN_USER" ]] || die "empty VPN username from $CREDENTIAL_PASS_ENTRY"
	[[ -n "$VPN_PASS" ]] || die "empty VPN password from $CREDENTIAL_PASS_ENTRY"

	if [[ "$VPN_USER" != *+* ]]; then
		warn "VPN username does not contain '+'. Verify $CREDENTIAL_PASS_ENTRY is the Proton OpenVPN/IKEv2 credential entry."
	fi
}

delete_if_exists() {
	local name=$1

	if connection_exists "$name"; then
		if "$DRY_RUN"; then
			info "would delete existing profile: $name"
		else
			nmcli connection delete "$name" >/dev/null
			info "deleted existing profile: $name"
		fi
	fi
}

configure_profile() {
	local profile=$1

	if "$DRY_RUN"; then
		if "$SET_SECRETS"; then
			info "would configure profile and store credentials: $profile"
		else
			info "would configure profile without credentials: $profile"
		fi
		return 0
	fi

	if "$SET_SECRETS"; then
		nmcli connection modify "$profile" \
			connection.autoconnect no \
			vpn.user-name "$VPN_USER" \
			vpn.secrets "password=$VPN_PASS" \
			+vpn.data password-flags=0
	else
		nmcli connection modify "$profile" connection.autoconnect no
	fi

	info "configured: $profile"
}

import_profile() {
	local entry=$1
	local profile=$2
	local base cfg import_log imported_name

	base=${entry%.tcp}

	if "$REPLACE"; then
		delete_if_exists "$profile"
		[[ "$base" != "$profile" ]] && delete_if_exists "$base"
	fi

	if connection_exists "$profile"; then
		info "exists: $profile"
		configure_profile "$profile"
		return 0
	fi

	if connection_exists "$base"; then
		if "$DRY_RUN"; then
			info "would rename existing profile: $base -> $profile"
		else
			nmcli connection modify "$base" connection.id "$profile"
			info "renamed: $base -> $profile"
		fi
		configure_profile "$profile"
		return 0
	fi

	if "$DRY_RUN"; then
		info "would import: $OVPN_PASS_PREFIX/$entry -> $profile"
		configure_profile "$profile"
		return 0
	fi

	cfg=$(mktemp "$TMP_DIR/${base}.XXXXXX.ovpn")
	import_log=$(mktemp "$TMP_DIR/${base}.import.XXXXXX.log")

	pass show "$OVPN_PASS_PREFIX/$entry" >"$cfg"
	chmod 600 "$cfg"

	if nmcli connection import type openvpn file "$cfg" >"$import_log" 2>&1; then
		info "imported: $base"
	else
		if connection_exists "$base"; then
			warn "nmcli import reported a failure, but $base exists; continuing"
		else
			warn "nmcli import failed for $entry"
			sanitize_log "$import_log"
			return 1
		fi
	fi

	if connection_exists "$profile"; then
		imported_name=$profile
	elif connection_exists "$base"; then
		imported_name=$base
	else
		warn "could not find imported profile for $entry"
		sanitize_log "$import_log"
		return 1
	fi

	if [[ "$imported_name" != "$profile" ]]; then
		nmcli connection modify "$imported_name" connection.id "$profile"
		info "renamed: $imported_name -> $profile"
	fi

	configure_profile "$profile"
}

main() {
	local row entry profile code country

	parse_args "$@"

	if "$LIST_ONLY"; then
		list_profiles
		exit 0
	fi

	need_cmd pass
	need_cmd nmcli
	need_cmd mktemp
	need_cmd sed
	need_cmd tr

	build_selection
	((${#SELECTED_PROFILES[@]} > 0)) || die "no profiles selected"

	if "$SET_SECRETS" && ! "$DRY_RUN"; then
		load_credentials
	elif "$SET_SECRETS" && "$DRY_RUN"; then
		info "dry-run: not reading credential secret from pass"
	fi

	TMP_DIR=$(mktemp -d)
	trap 'rm -rf "$TMP_DIR"' EXIT

	for row in "${SELECTED_PROFILES[@]}"; do
		IFS='|' read -r entry profile code country <<<"$row"
		import_profile "$entry" "$profile"
	done

	info "done"
}

main "$@"
