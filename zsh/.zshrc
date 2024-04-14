export ZDOTDIR="$(dirname "$(readlink -f "${(%):-%x}")")"
export GUIX_PROFILE="$HOME/.guix-profile"
source "$GUIX_PROFILE/etc/profile"

zsh_dir="$ZDOTDIR"

HISTFILE="$zsh_dir/zsh_history"
HISTSIZE=10000
SAVEHIST=10000

source "$zsh_dir/lscolors.sh"

function pretty_env() {
    env | grep -v '^LS_COLORS=' | sort | awk -F '=' '{printf "%-30s %s\n", $1, $2}'
}

autoload -Uz colors && colors
PROMPT='%F{#ff6c6b}カオス%f %F{#98be65}%2~%f %F{#51afef}>%f '
RPROMPT='%F{#88c0d0}%D{%d %b}%f %F{#ECBE7B}%D{%l:%M:%S}%f'

function update_clock_precmd {
    RPROMPT="%F{#88c0d0}%D{%d %b}%f %F{#ECBE7B}%D{%l:%M:%S}%f"
}
autoload -Uz add-zsh-hook
add-zsh-hook precmd update_clock_precmd

TRAPALRM() {
    zle reset-prompt
}

TMOUT=1

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias env='pretty_env'
alias e='emacs -nw'
alias zshrc="emacs -nw $zsh_dir/.zshrc"
alias reconfigure='sudo guix system reconfigure $zsh_dir/config.scm'

setopt NO_BEEP
setopt EXTENDED_GLOB
setopt NO_CLOBBER
setopt SHARE_HISTORY

export EDITOR="emacs"

umask 022

if [ -n "$GUIX_ENVIRONMENT" ]; then
    PROMPT='%F{#ff6c6b}カオス%f %F{#98be65}%2~%f %F{#51afef}>%f '
fi

[[ -z "$(whence -p compinit)" ]] && autoload -Uz compinit && compinit

function create_perl_script_aliases() {
  local script_dir="$zsh_dir/../perl-scripts"

  for script in "$script_dir"/*.pl; do
    local script_name="$(basename "${script%.pl}")"
    alias "$script_name=perl $script"
  done
}

function create_bash_script_aliases() {
  local script_dir="$zsh_dir/../bash-scripts"

  for script in "$script_dir"/*.sh; do
    local script_name="$(basename "${script%.sh}")"
    alias "$script_name=bash $script"
  done
}
