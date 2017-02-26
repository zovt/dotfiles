## Options ##
autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

setopt COMPLETE_ALIASES

# History
export HISTFILE=~/.history
export HISTSIZE=100000000
export SAVEHIST=$HISTSIZE
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
alias cdz='cd $(find . | fzf)'
alias cz='c $(fzf)'
alias clz='c $(fzf) | less'

# Colorized cat
alias c='highlight -O ansi'

# NVIM aliases
alias e='nvim'
alias ez='nvim $(fzf)'

# Tree
alias t='tree'
alias tl='tree | less'

## Other (probably autoappended) ##
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH=$PATH:$HOME/.cargo/bin/
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
