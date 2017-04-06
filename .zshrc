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
alias g='git'
compdef g='git'

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

# Ripgrep FZF
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

# NVIM as Editor
export EDITOR=nvim

# Source zshenv
source ~/.zshenv

## Other (probably autoappended) ##
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH=$PATH:$HOME/.cargo/bin/
