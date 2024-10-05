if status is-interactive
    # Disable greeting
    set fish_greeting
end

# XDG directories
set -qx XDG_DATA_HOME; or set -Ux XDG_DATA_HOME $HOME/.local/share
set -qx XDG_CONFIG_HOME; or set -Ux XDG_CONFIG_HOME $HOME/.config
set -qx XDG_STATE_HOME; or set -Ux XDG_STATE_HOME $HOME/.local/state
set -qx XDG_CACHE_HOME; or set -Ux XDG_CACHE_HOME $HOME/.cache

# Path
fish_add_path -m $HOME/.local/bin
fish_add_path $HOME/.cargo/bin
fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/sbin
fish_add_path /usr/local/sbin

# I don't use home-manager to take over shell configuration
# fenv source /etc/profiles/per-user/brunetdragon/etc/profile.d/hm-session-vars.sh
# NOTE `.nix-profile` symlink somehow point to invalid path
# redirect `.nix-profile` to `/etc/static/profiles/per-user/brunetdragon`
# so I don't have to manually add path
fish_add_path /nix/var/nix/profiles/default/bin
fish_add_path /etc/profiles/per-user/brunetdragon/bin

set -Ux EDITOR nvim
set -Ux SUDO_EDITOR $EDITOR
set -Ux GIT_EDITOR $EDITOR
set -Ux VISUAL $EDITOR
set -Ux MANPAGER 'nvim +Man!'
set -Ux LESS '-FiQMXRwJ --incsearch --status-col-width 1'
set -Ux LESSCHARSET UTF-8
set -Ux PAGER less

set -Ux GOKU_EDN_CONFIG_FILE $HOME/nix/dotfiles/karabiner/karabiner.edn

set -Ux SSH_KEY_PATH $HOME/.ssh/id_rsa
set -gx SSH_AUTH_SOCK $SSH_AUTH_SOCK
set -gx SSH_AGENT_PID $SSH_AGENT_PID

set -gx OPENAI_API_KEY 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
set -gx CACHIX_AUTH_TOKEN eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiJmOGEwNTI5Ni1kNjUwLTQ1MDktOGRkNy1kZTI2ZDVjM2NkZDUiLCJzY29wZXMiOiJjYWNoZSJ9.SA7Sf9Uq0iBHI4VtfKjVR87tpP3fPYB6jQ7BSGJIwi8

set -x LC_ALL en_US.UTF-8
set -x LC_CTYPE en_US.UTF-8

# Cursor styles
set -gx fish_vi_force_cursor 1
set -gx fish_cursor_default block
set -gx fish_cursor_insert line blink
set -gx fish_cursor_visual block
set -gx fish_cursor_replace_one underscore

if command -qs fzf
    set -Ux FZF_DEFAULT_OPTS "\
    --color=bg+:#1e1e2e,bg:#14141f,spinner:#f5e0dc,hl:#f38ba8 \
    --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
    --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8,separator:#f5e0dc"
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
    abbr vim nvim
    abbr vi nvim
    abbr v nvim
    abbr nv nvim
    abbr sv sudoedit
    abbr vudo sudoedit
    alias f 'nvim $(fzf)'
end

abbr .... 'cd ../../../'
abbr ... 'cd ../../'
abbr .. 'cd ..'
abbr . pwd
abbr c clear
abbr cp 'cp -riv'
abbr mv 'mv -iv'
abbr rm 'rm -rf'
abbr mkdir 'mkdir -pv'
abbr top vtop
abbr oldtop /usr/bin/top

if command -qs tmux
    abbr t tmux
    abbr tc 'tmux attach'
    abbr ta 'tmux attach -t'
    abbr tad 'tmux attach -d -t'
    abbr ts 'tmux new -s'
    abbr tl 'tmux ls'
    abbr tk 'tmux kill-session -t'
end

# Git
if command -qs git
    alias g git
    alias lazygit "TERM=xterm-256color command lazygit"
    abbr gg lazygit
    abbr gl 'hub l --color | devmoji --log --color | less -rXF'
    abbr gs "hub st"
    abbr gb "hub checkout -b"
    abbr gc "hub commit"
    abbr gpr "hub pr checkout"
    abbr gm "hub branch -l main | rg main > /dev/null 2>&1 && hub checkout main || hub checkout master"
    abbr gcp "hub commit -p"
    abbr gpp "hub push"
    abbr gp "hub pull"
end

# Exa
if command -qs eza
    alias ls="eza --color=always --icons --group-directories-first"
    alias la 'eza --color=always --icons --group-directories-first --all'
    alias ll 'eza --color=always --icons --group-directories-first --all --long'
    abbr l ll
    # automatically call exa on cd
    functions --copy cd standard_cd
    function cd
        standard_cd $argv; and exa -G --color auto -a -s type
    end
end

if command -qs zoxide
  zoxide init fish --cmd j | source
end

# macOS
alias disableani 'defaults write com.apple.finder DisableAllAnimations -bool true'
alias disablekeys 'defaults write -g ApplePressAndHoldEnabled -bool false'
alias enableani 'defaults write com.apple.finder DisableAllAnimations -bool false'
alias enablekeys 'defaults write -g ApplePressAndHoldEnabled -bool true'
alias hide 'defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'
alias show 'defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias rmds 'fd -H -I \.DS\_Store -x rm -v'
alias rmconflict 'fd --hidden conflicted ~/Dropbox'
alias wezfont 'wezterm ls-fonts --list-system'
