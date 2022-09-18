# Rose Pine Term Colors Default
white=#fff
red=#ff5555
yellow=#f1fa8c
cyan=#8be9fd
green=#50fa7b
blue=#bd93f9
magenta=#ff79c6

# default window title colors
# set-option -g status-style fg=magenta,bg=default

set -g status on
set -g status-interval 3

# Left Status Bar
set -g status-left-length 80
set -g status-left '#[fg=magenta,bg=default,nobold] #S #[fg=cyan,bg=default,nobold] '

# Right Status Bar
set -g status-right-length 150
set -g status-right '#[fg=black] #[fg=magenta,bg=default] #{pane_current_path} #[fg=magenta,bg=default] #[fg=blue,bg=default] #[fg=cyan,bg=default,bold] %H:%M %a-%m-%d-%Y'
# Current Tab
set -g window-status-current-format '#[fg=default,bg=default] #[fg=red,bg=default,bold] #I #[fg=red,nobold]#W #[fg=blue,bg=default,nobold] '

# Inactive Tab
set -g window-status-format '#[fg=blue,bg=default] #[fg=cyan,bg=default] #I #W #[fg=cyan,bg=default]#[fg=blue,bg=default,nobold] '

# Activity Tab
set-window-option -ga window-status-activity-style fg=blue,bg=default

# Separator
set -g window-status-separator ''

# Highlight zoomed windows
setw -g window-status-current-format '#{?window_zoomed_flag,#[fg=magenta],}#F#I [#W] '
