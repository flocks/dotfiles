# common
#
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'
export EDITOR="$(command -v nvim)"
export TERMINAL="$(command -v kitty)"
export BROWSER="$(command -v google-chrome-stable)"
# PATH
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/endorsement/bin
export PATH=~/.npm-global/bin:$PATH
export PATH=~/.yarn/bin:$PATH
export PATH=~/.local/share/gem/ruby/3.0.0/bin:$PATH


export LANG=en_US.UTF-8
#disable bell
unsetopt BEEP
#oh-my-zsh
export ZSH=$HOME/.oh-my-zsh
# shellcheck disable=SC2034
CASE_SENSITIVE="true"
# shellcheck disable=SC2034
plugins=()
source "$ZSH/oh-my-zsh.sh"

#fzf
export FZF_DEFAULT_COMMAND='rg --files --follow'
export FZF_CTRL_T_COMMAND='rg --files --follow 2>/dev/null'
export FZF_ALT_C_COMMAND="fd --type directory"



# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
fzfKeyBindings=/usr/share/fzf/key-bindings.zsh
if [ -f $fzfKeyBindings ]; then
  source $fzfKeyBindings
fi

#ssh
if [ -z "$SSH_AUTH_SOCK" ] ; then
  echo "> Starting ssh-agent"
  eval $(ssh-agent -s)
fi

if [ -n "$DISPLAY" ] && ! ssh-add -l >/dev/null; then
  ssh-add
fi

source "$HOME/.alias"
source "$HOME/.path"
source "$HOME/.gitprompt"
source "$HOME/.prompt"
source "$HOME/.private"


# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^o' 'lfcd\n'
# commment because can use C-x C-e and keeps C-e for end of line
# bindkey '^e' edit-command-line
# Use lf to switch directories and bind it to ctrl-o
# bindkey -s '^[g' 'nvim +":Git"\n'
# bindkey -s '^[s' 'git status\n'
# bindkey -s '^[a' 'git add '
# bindkey -s '^[D' 'git diff\n'

bindkey -s '^x^j' 'nvim .\n'
bindkey -s '^d' 'cd $(fd --type directory | fzf) \n'
bindkey -s '^[g' 'nvim +":Git"\n'


source "$HOME/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
#
# useful for pinentry-tty prompt
export GPG_TTY="$(tty)"

setopt PROMPT_SUBST
PATH="/home/flocks/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/flocks/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/flocks/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/flocks/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/flocks/perl5"; export PERL_MM_OPT;


