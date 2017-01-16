autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

setopt COMPLETE_ALIASES

autoload -Uz promptinit
promptinit
prompt suse

# Git
alias gs='git status'
alias gc='git commit -a'
alias gcm='git commit -am'

# FZF
alias fzcd='cd $(find . | fzf)'
alias fznv='nvim $(fzf)'


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
