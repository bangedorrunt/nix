#!/usr/bin/zsh
#          ^----- get shellcheck hints based on bash
# https://github.com/koalaman/shellcheck/issues/809

# Enable powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Handle macOS platforms
# ----------------------
CPU=$(uname -p)
if [[ "$CPU" == "arm" ]]; then
	export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
else
  export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
fi

# Editor and pager
# ----------------
export EDITOR=nvim
alias v='nvim'
alias vi='nvim'
alias vim='nvim'
export MANPAGER='nvim +Man!'
export GIT_EDITOR="$EDITOR"
export VISUAL="$EDITOR"
export PAGER=less
export LESS="-FiQMXRwJ --incsearch --status-col-width 1"
export LESSCHARSET="UTF-8"

# SSH
# ---
export SSH_KEY_PATH="~/.ssh/id_rsa"

# Fzf
# ---
# `rose-pine` theme
# --color=fg:#e0def4,bg:#1f1d2e,hl:#6e6a86
# --color=fg+:#908caa,bg+:#191724,hl+:#908caa
# --color=info:#9ccfd8,prompt:#f6c177,pointer:#c4a7e7
# --color=marker:#ebbcba,spinner:#eb6f92,header:#ebbcba"

export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS"
--color=fg:#e0def4,bg:-1,hl:#6e6a86
--color=fg+:#908caa,bg+:-1,hl+:#908caa
--color=info:#9ccfd8,prompt:#f6c177,pointer:#c4a7e7
--color=marker:#ebbcba,spinner:#eb6f92,header:#ebbcba"

# RG as default
export FZF_DEFAULT_COMMAND="rg --files --follow --hidden --glob '!{.git,node_modules}/**'"
export FZF_CTRL_T_COMMAND="rg --files --follow --hidden --glob '!{.git,node_modules}/**'"
export FZF_ALT_C_COMMAND="fd --type d --no-ignore-vcs --exclude node_modules --exclude .git"

alias f='nvim $(fzf)'

# Nvm
# ---
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# Zoxide
# ------
eval "$(zoxide init zsh --cmd j)"

# Goku
# ----
export GOKU_EDN_CONFIG_FILE="$HOME/nix/dotfiles/karabiner/karabiner.edn"

# Added by zinit's installer
# --------------------------
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

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

# Fzf completion
zinit wait lucid light-mode for \
  id-as'fzf/completion' https://github.com/junegunn/fzf/blob/master/shell/completion.zsh \
  id-as'fzf/key-bindings' https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh

# Fast syntax highlighting & autosuggestions
zinit wait"1" lucid for \
  atinit"ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" \
    zdharma-continuum/fast-syntax-highlighting \
  atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions \
  blockf \
    zsh-users/zsh-completions
# Fast navigation
zinit wait"1" lucid for \
    zdharma-continuum/zsh-navigation-tools \
    zsh-users/zsh-history-substring-search \
  atinit'zstyle ":history-search-multi-word" page-size "7"' \
    zdharma-continuum/history-search-multi-word
# End of zinit's installer chunk

# Aliases
# -------

# Git
alias g='git'
alias gs='git status'
alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gd='git diff'
alias gco='git checkout'

alias got='git '
alias get='git '

# Exa
if [ "$(command -v exa)" ]; then
    unalias -m 'll'
    unalias -m 'l'
    unalias -m 'la'
    unalias -m 'ls'
    alias ls='exa -G  --color auto -a -s type'
    alias ll='exa -l --color always -a -s type'
fi

# Bat
if [ "$(command -v bat)" ]; then
  unalias -m 'cat'
  alias cat='bat -pp'
fi

# CLI
alias ....='cd ../../../'
alias ...='cd ../../'
alias ..='cd ..'
alias .='pwd'
alias c='clear'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -rf'
alias mkdir='mkdir -pv'
alias path='echo $PATH | tr -s ":" "\n"'
alias top='vtop'
alias oldtop='/usr/bin/top'

alias lg='lazygit'
alias t='tmux'
alias ta='tmux attach -t'
alias td='tmux detach'
alias tk='tmux kill-server'

# macOS
alias disableani='defaults write com.apple.finder DisableAllAnimations -bool true'
alias disablekeys='defaults write -g ApplePressAndHoldEnabled -bool false'
alias enableani='defaults write com.apple.finder DisableAllAnimations -bool false'
alias enablekeys='defaults write -g ApplePressAndHoldEnabled -bool true'
alias flushdns='sudo dscacheutil -flushcache;sudo killall -HUP mDNSResponder;echo "✌️ DNS flushed"'
alias hide='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'
alias show='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
alias rmds='fd -H -I \.DS\_Store -x rm -v'
alias rmconflict='fd --hidden conflicted ~/Dropbox'
alias nixup="sudo -i sh -c 'nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'"
alias nvup="brew reinstall neovim"
alias wezup="brew upgrade --cask wezterm-nightly --no-quarantine --greedy-latest"
alias wezfont="wezterm ls-fonts --list-system"
