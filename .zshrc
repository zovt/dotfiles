## Options ##
autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

setopt COMPLETE_ALIASES

# History
setopt inc_append_history
setopt hist_ignore_dups
setopt hist_ignore_space

# Prompt
autoload -Uz promptinit
promptinit
prompt suse

## Aliases ##
# Git
alias gs='git status'
alias gc='git commit -a'
alias gcm='git commit -am'
alias gp='git push'
alias gpl='git pull'
alias gb='git branch'
alias gch='git checkout'

# FZF
alias fzcd='cd $(find . | fzf)'
alias fzc='c $(fzf)'

# Colorized cat
alias c='highlight -O ansi'

# NVIM aliases
alias e='nvim'
alias ez='nvim $(fzf)'

## Other (probably autoappended) ##
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
