#!/bin/zsh
#      ^----- get shellcheck hints based on bash
# https://github.com/koalaman/shellcheck/issues/809
# shellcheck disable=SC1090 # sourced filenames with variables

# If you come from bash you might have to change your $PATH.
# export PATH=/usr/local/bin:/usr/local/sbin:$HOME/bin:$PATH

# Enable powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# FZF CONFIG
# ######################

# Iceberg Light: e8e9ec
# Onehalf: 282c34
# Onehalf Light: fafafa
# Github Android: 17181c
# Github Dark: 1e2429
# Sonokai: 2c2e33
# Tokyonight: e1e2e7

# Dark
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS' --color=fg:#e1e2e7,bg:#1a1b26,hl:#5f87af --color=fg+:#e1e2e7,bg+:#1a1b26,hl+:#5fd7ff --color=info:#3f83a6,prompt:#d7005f,pointer:#af5fff --color=marker:#87ff00,spinner:#af5fff,header:#87afaf'

# Light
# export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS' --color=fg:#33374c,bg:#e1e2e7,hl:#5f87af --color=fg+:#33374c,bg+:#e1e2e7,hl+:#5fd7ff --color=info:#3f83a6,prompt:#d7005f,pointer:#af5fff --color=marker:#87ff00,spinner:#af5fff,header:#87afaf'
# export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_DEFAULT_COMMAND="rg --files --follow --hidden --glob '!{.git,node_modules}/**'"
export FZF_CTRL_T_COMMAND="rg --files --follow --hidden --glob '!{.git,node_modules}/**'"
export FZF_ALT_C_COMMAND="fd --type d --no-ignore-vcs --exclude node_modules --exclude .git"

alias fv='nvim $(fzf)'

# ZOXIDE CONFIG
# ######################
eval "$(zoxide init zsh --cmd j)"

# GOKU
# #####################
export GOKU_EDN_CONFIG_FILE="$HOME/.config/karabiner/karabiner.edn"

### Added by zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node

# A glance at the new for-syntax – load all of the above
# plugins with a single command. For more information see:
# https://zdharma.org/zinit/wiki/For-Syntax/

# powerlevel10k theme
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
zinit light-mode for atload'source ~/.p10k.zsh' romkatv/powerlevel10k

# Sensible defaults
zstyle ':prezto:*:*' color 'yes'
zinit light-mode for \
  id-as'prezto/environment' \
    https://github.com/sorin-ionescu/prezto/blob/master/modules/environment/init.zsh \
  id-as'prezto/history' \
    https://github.com/sorin-ionescu/prezto/blob/master/modules/history/init.zsh \
  id-as'prezto/directory' \
    https://github.com/sorin-ionescu/prezto/blob/master/modules/directory/init.zsh
HISTSIZE=200000
SAVEHIST=100000

# Vi mode
zinit wait lucid light-mode for OMZP::vi-mode

# FZF completion
zinit wait lucid light-mode for \
  id-as'fzf/completion' https://github.com/junegunn/fzf/blob/master/shell/completion.zsh \
  id-as'fzf/key-bindings' https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh

# Fast syntax highlighting & autosuggestions
zinit wait"1" lucid for \
  atinit"ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" \
    zdharma/fast-syntax-highlighting \
  atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions \
  blockf \
    zsh-users/zsh-completions
# Fast navigation
zinit wait"1" lucid for \
    psprint/zsh-navigation-tools \
    zsh-users/zsh-history-substring-search \
  atinit'zstyle ":history-search-multi-word" page-size "7"' \
    zdharma/history-search-multi-word

### End of zinit's installer chunk

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# SSH
export SSH_KEY_PATH="~/.ssh/rsa_id"


alias v='nvim'
alias vi='nvim'
alias vim='nvim'

# alias code='code-insiders'
# alias c='code-insiders'
alias desktop='cd ~/Desktop'
if [ "$(command -v exa)" ]; then
    unalias -m 'll'
    unalias -m 'l'
    unalias -m 'la'
    unalias -m 'ls'
    alias ls='exa -G  --color auto -a -s type'
    alias ll='exa -l --color always -a -s type'
fi

if [ "$(command -v bat)" ]; then
  unalias -m 'cat'
  alias cat='bat -pp'
fi

# alias rm='trash'
# alias rmds='find . -type f -name '*.DS_Store' | xargs rm'
alias ....='cd ../../../'
alias ...='cd ../../'
alias ..='cd ..'
alias .='pwd'
alias cl='clear'
alias disableani='defaults write com.apple.finder DisableAllAnimations -bool true'
alias disablekeys='defaults write -g ApplePressAndHoldEnabled -bool false'
alias enableani='defaults write com.apple.finder DisableAllAnimations -bool false'
alias enablekeys='defaults write -g ApplePressAndHoldEnabled -bool true'
alias flushdns='sudo dscacheutil -flushcache;sudo killall -HUP mDNSResponder;echo "✌️ DNS flushed"'
alias hide='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'
alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
alias mkdir='mkdir -pv'
alias nixup="sudo -i sh -c 'nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'"
alias oldtop='/usr/bin/top'
alias path='echo $PATH | tr -s ":" "\n"'
alias rmconflict='fd --hidden conflicted ~/Dropbox'
alias rmds='fd -H -I \.DS\_Store -x rm -v'
alias rsync='rsync -a --delete'
alias show='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias top='vtop'


## Alias for neuron
function ns() {
  if [[ "$PWD" =~ "notetoself" ]]; then
    nvim $(neuron search)
  else 
    echo 'Gotta go to "notetoself"'
  fi
}
function nn() {
  if [[ "$PWD" =~ "notetoself" ]]; then
    neuron new
  else 
    echo 'Gotta go to "notetoself"'
  fi
}
