# Disable greeting
set fish_greeting

set -g async_prompt_functions _pure_prompt_git

# XDG directories
set -Ux XDG_DATA_HOME $HOME/.local/share
set -Ux XDG_CONFIG_HOME $HOME/.config
set -Ux XDG_STATE_HOME $HOME/.local/state
set -Ux XDG_CACHE_HOME $HOME/.cache

# Path
set -Ux PATH $HOME/.local/bin $PATH
set -Ux PATH $HOME/.cargo/bin $PATH
set -Ux PATH /opt/homebrew/bin $PATH
set -Ux PATH /opt/homebrew/sbin $PATH

set -Ux EDITOR nvim
set -Ux MANPAGER 'nvim +Man!'
set -Ux GIT_EDITOR $EDITOR
set -Ux VISUAL $EDITOR
set -Ux PAGER less
set -Ux LESS '-FiQMXRwJ --incsearch --status-col-width 1'
set -Ux LESSCHARSET "UTF-8"

set -Ux GOKU_EDN_CONFIG_FILE $HOME/nix/dotfiles/karabiner/karabiner.edn

set -Ux SSH_KEY_PATH $HOME/.ssh/id_rsa

set -x LC_ALL en_US.UTF-8
set -x LC_CTYPE en_US.UTF-8

if command -qs fzf
  set -Ux FZF_DEFAULT_OPTS $FZF_DEFAULT_OPTS " --color=fg:#e0def4,bg:-1,hl:#6e6a86 --color=fg+:#908caa,bg+:-1,hl+:#908caa --color=info:#9ccfd8,prompt:#f6c177,pointer:#c4a7e7 --color=marker:#ebbcba,spinner:#eb6f92,header:#ebbcba"
  set -Ux FZF_DEFAULT_COMMAND "rg --files --follow --hidden --glob '!{.git,node_modules}/**'"
  set -Ux FZF_CTRL_T_COMMAND "rg --files --follow --hidden --glob '!{.git,node_modules}/**'"
  set -Ux FZF_ALT_C_COMMAND "fd --type d --no-ignore-vcs --exclude node_modules --exclude .git"

  if functions -q fzf_configure_bindings
    set -g fzf_dir_opts --header="Search file"
    set -g fzf_git_log_opts --header="Git logs"
    set -g fzf_git_status_opts --header="Git status"
    set -g fzf_shell_vars_opts --header="Search shell variables"
    set -g fzf_processes_opts --header="Search processes"
    set -g fzf_history_opts --header="Search fish history"

    set fzf_fd_opts --hidden --exclude=.git

    # Use exa if possible for directory preview command
    if command -qs exa
      set fzf_preview_dir_cmd exa --all --color=always
    end

    # Use cat if bat is not available
    if not command -qs bat
      set fzf_preview_file_cmd cat
    end
  end

end

if command -qs nvim
  alias v 'nvim'
  alias nv 'nvim'
  alias f 'nvim $(fzf)'
end

alias .... 'cd ../../../'
alias ... 'cd ../../'
alias .. 'cd ..'
alias . 'pwd'
alias c 'clear'
alias cp 'cp -i'
alias mv 'mv -i'
alias rm 'rm -rf'
alias mkdir 'mkdir -pv'
alias path 'echo $PATH | tr -s ":" "\n"'
alias top 'vtop'
alias oldtop '/usr/bin/top'

if command -qs tmux
  alias t 'tmux'
  abbr -a ta tmux attach -t
  abbr -a td tmux detach
  abbr -a tk tmux kill-server
end

# Git
if command -qs git
  alias g 'git'
  abbr -a gcl git clone
  abbr -a gst git status
  abbr -a gsts git status --short
  abbr -a gb git branch
  abbr -a gsw git switch
  abbr -a gswc git switch --create
  abbr -a gco git checkout
  abbr -a gcob git checkout -b
  abbr -a ga git add
  abbr -a gd git diff
  abbr -a gds git diff --staged
  abbr -a gc git commit
  abbr -a gl git pull
  abbr -a gp git push
  abbr -a gr git restore
  abbr -a grs git restore --staged
  abbr -a gwa git worktree add
  abbr -a gwp git worktree prune
end

# Exa
if command -qs exa
  alias ls 'exa -G  --color auto -a -s type'
  alias ll 'exa -l --color always -a -s type'
  # automatically call exa on cd
  functions --copy cd standard_cd
  function cd
    standard_cd $argv; and exa -G  --color auto -a -s type
  end
end
# macOS
alias disableani 'defaults write com.apple.finder DisableAllAnimations -bool true'
alias disablekeys 'defaults write -g ApplePressAndHoldEnabled -bool false'
alias enableani 'defaults write com.apple.finder DisableAllAnimations -bool false'
alias enablekeys 'defaults write -g ApplePressAndHoldEnabled -bool true'
alias flushdns 'sudo dscacheutil -flushcache;sudo killall -HUP mDNSResponder;echo "✌️ DNS flushed"'
alias hide 'defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'
alias show 'defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias rmds 'fd -H -I \.DS\_Store -x rm -v'
alias rmconflict 'fd --hidden conflicted ~/Dropbox'
alias nvup 'brew reinstall neovim'
alias wezup 'brew upgrade --cask wezterm-nightly --no-quarantine --greedy-latest'
alias wezfont 'wezterm ls-fonts --list-system'
