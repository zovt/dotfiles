autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

setopt COMPLETE_ALIASES

autoload -Uz promptinit
promptinit
prompt suse

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
