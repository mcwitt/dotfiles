# Antigen config
## Load Antigen
source ~/code/dotfiles/antigen/antigen.zsh

## Core library
antigen use oh-my-zsh

## Theme
antigen theme robbyrussell/oh-my-zsh themes/agnoster

## Antigen bundles
antigen bundles <<EOBUNDLES
colored-man-pages
command-not-found
git
pip
python
tmux
ssh-agent
virtualenv
web-search
zsh-users/zsh-autosuggestions
zsh-users/zsh-completions
zsh-users/zsh-history-substring-search
zsh-users/zsh-syntax-highlighting
rupa/z
EOBUNDLES

## OS-specific bundles
if [[ $CURRENT_OS == 'OS X' ]]; then
    antigen bundle brew
    antigen bundle brew-cask
    antigen bundle gem
    antigen bundle osx
fi

## Tell antigen we're done
antigen apply

# Directory changing
setopt AUTO_CD

# History
HISTFILE=~/.histfile
HISTSIZE=1000
setopt APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS

# Background jobs
setopt NOTIFY  # notify of background job status changes

# Key bindings
setopt VI  # use VI bindings
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Miscellaneous
setopt CORRECT
alias rm="rm -i"