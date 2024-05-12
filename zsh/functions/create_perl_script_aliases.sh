# Dynamically creates aliases for all Perl scripts in ~/perl-scripts
function create_perl_script_aliases {
    local script_dir="$HOME/perl-scripts"
    for script in "$script_dir"/*.pl; do
        local script_name="$(basename "${script%.pl}")"
        alias "$script_name=perl $script"
    done
}
