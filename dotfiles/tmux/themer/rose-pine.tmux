# Rose Pine Term Colors Default
white=#fff
red=#ff5555
yellow=#f1fa8c
cyan=#8be9fd
green=#50fa7b
blue=#bd93f9
magenta=#ff79c6

# default window title colors
set-option -g status-style fg=magenta,bg=#191724

set -g status on
set -g status-interval 3

# Left Status Bar
set -g status-left-length 80
set -g status-left '#[fg=#191724,bg=magenta,nobold] #S #[fg=cyan,bg=#191724,nobold] '

# Right Status Bar
set -g status-right-length 150
set -g status-right '#[fg=black] #[fg=magenta,bg=black] #{pane_current_path} #[fg=#191724,bg=black] #[fg=black,bg=#191724] #[fg=brightblack,bg=black,bold] %H:%M %a-%m-%d-%Y'
# Current Tab
set -g window-status-current-format '#[fg=#191724,bg=black] #[fg=red,bg=black,bold] #I #[fg=red,nobold]#W #[fg=black,bg=#191724,nobold] '

# Inactive Tab
set -g window-status-format '#[fg=#191724,bg=black] #[fg=brightblack,bg=black] #I #W #[fg=brightblack,bg=black]#[fg=black,bg=#191724,nobold] '

# Activity Tab
set-window-option -ga window-status-activity-style fg=blue,bg=white

# Separator
set -g window-status-separator ''

# Highlight zoomed windows
setw -g window-status-current-format '#{?window_zoomed_flag,#[fg=magenta],}#F#I [#W] '
