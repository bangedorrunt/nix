#!/bin/zsh

# if you come from bash you might have to change your $PATH.
export PATH=/usr/local/bin:/usr/local/sbin:$HOME/bin:$PATH

# enable powerlevel10k instant prompt. should stay close to the top of ~/.zshrc.
# initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# fzf config
# ######################

# iceberg-light: e8e9ec
# onehalf: 282c34
# onehalf light: fafafa
# github android: 17181c
# github dark: 1e2429
# sonokai: 2c2e33

# dark
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS' --color=fg:#24292e,bg:#17181c,hl:#5f87af --color=fg+:#24292e,bg+:#17181c,hl+:#5fd7ff --color=info:#3f83a6,prompt:#d7005f,pointer:#af5fff --color=marker:#87ff00,spinner:#af5fff,header:#87afaf'

# light
# export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS' --color=fg:#33374c,bg:#e8e9ec,hl:#5f87af --color=fg+:#33374c,bg+:#e8e9ec,hl+:#5fd7ff --color=info:#3f83a6,prompt:#d7005f,pointer:#af5fff --color=marker:#87ff00,spinner:#af5fff,header:#87afaf'
# export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_DEFAULT_COMMAND="rg --files --follow --no-ignore-vcs --hidden --glob '!{.git,node_modules}/**'"
export FZF_CTRL_T_COMMAND="rg --files --follow --no-ignore-vcs --hidden --glob '!{.git,node_modules}/**'"
export FZF_ALT_C_COMMAND="fd --type d --no-ignore-vcs --exclude node_modules --exclude .git"

# nvm config
# ######################
export NVM_DIR="$HOME/.nvm"
export NVM_SYMLINK_CURRENT=true
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nv

# z config
# ######################
. /usr/local/etc/profile.d/z.sh # This load z

# goku
# #####################
export GOKU_EDN_CONFIG_FILE="$HOME/.config/karabiner/karabiner.edn"

# homebrew config
# ######################
# this load brew wrapper
if [ -f $(brew --prefix)/etc/brew-wrap ];then
  source $(brew --prefix)/etc/brew-wrap
fi

# added by Nix installer
# if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi

### added by zinit's installer
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

# load a few important annexes, without turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node

# a glance at the new for-syntax – load all of the above
# plugins with a single command. For more information see:
# https://zdharma.org/zinit/wiki/For-Syntax/

# powerlevel10k theme
# to customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
zinit light-mode for atload'source ~/.p10k.zsh' romkatv/powerlevel10k

# sensible defaults
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

# vi mode
zinit wait lucid light-mode for OMZP::vi-mode

# fzf completion
zinit wait lucid light-mode for \
  id-as'fzf/completion' https://github.com/junegunn/fzf/blob/master/shell/completion.zsh \
  id-as'fzf/key-bindings' https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh

# pyenv lazy init
zinit wait lucid light-mode for davidparsson/zsh-pyenv-lazy

# auto-suggest how to install missing commands.
zinit light-mode for id-as'brew/command-not-found' \
  https://github.com/Homebrew/homebrew-command-not-found/blob/master/handler.sh

# fast-syntax-highlighting & autosuggestions
zinit wait"1" lucid for \
  atinit"ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" \
    zdharma/fast-syntax-highlighting \
  atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions \
  blockf \
    zsh-users/zsh-completions
# fast navigation
zinit wait"1" lucid for \
    psprint/zsh-navigation-tools \
    zsh-users/zsh-history-substring-search \
  atinit'zstyle ":history-search-multi-word" page-size "7"' \
    zdharma/history-search-multi-word

### end of zinit's installer chunk

# you may need to manually set your language environment
# export LANG=en_US.UTF-8

# preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vim'
fi

# compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# for a full list of active aliases, run `alias`.

alias v='nvim'
alias vi='nvim'
alias vim='nvim'
alias code='code-insiders'
alias c='code-insiders'
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
