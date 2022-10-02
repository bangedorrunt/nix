# Rose Pine Term Colors Default
white=#fff
red=#ff5555
yellow=#f1fa8c
cyan=#8be9fd
green=#50fa7b
blue=#bd93f9
magenta=#ff79c6

set -g status on
set -g status-interval 3

# Default window title colors
set-option -g status-style fg=#ff79c6,bg=default
set-window-option -g window-status-style fg=#bd93f9,bg=default
set-window-option -g window-status-current-style fg=#50fa7b,bg=default
set-window-option -ga window-status-activity-style fg=#bd93f9,bg=default

# Left Status Bar
set -g status-left-length 80
set -g status-left '#[bold]#S '

# Right Status Bar
set -g status-right-length 150
set -g status-right '#[fg=#f1fa8c]#{pane_current_path} #[fg=#ff5555]%H:%M %a-%m-%d-%Y'

# Current Window
set -g window-status-current-format '#[bold][#I #W]'

# Inactive Window
set -g window-status-format '[#I #W]'

# Separator
# set -g window-status-separator ''
