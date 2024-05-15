# This function prints out environment variables in a readable, sorted format excluding LS_COLORS, using grep, sort, and awk.
function pretty_env() {
    env | grep -v '^LS_COLORS=' | sort | awk -F '=' '{printf "%-30s %s\n", $1, $2}'
}
