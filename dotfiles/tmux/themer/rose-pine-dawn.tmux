# Rose Pine Term Colors Default
white=#e0def4
red=#eb6f92
yellow=#f6c177
cyan=#ebbcba
green=#31748f
blue=#9ccfd8
magenta=#c4a7e7

# default window title colors
set-option -g status-style fg=white,bg=#191724

set -g status on
set -g status-interval 2
# set -Fg 'status-format[1]' '#{status-format[0]}'
# set -g 'status-format[0]' ''
# set -g status 2

# Left Status Bar
set -g status-left-length 60
set -g status-left '#[fg=#191724,bg=cyan,nobold]  #S #[fg=cyan,bg=#191724,nobold] '

# Right Status Bar
set -g status-right-length 150
set -g status-right '#[fg=black] #[fg=magenta,bg=black] #{pane_current_path} #[fg=#191724,bg=black] #[fg=black,bg=#191724] #[fg=brightblack,bg=black,bold] [%H:%M] %a-%m-%d-%Y #[fg=#191724,bg=black] #[fg=black,bg=#191724]#[fg=cyan] #[fg=black,bg=cyan,bold] #{battery_percentage} '

# Current Tab
set -g window-status-current-format '#[fg=#191724,bg=black] #[fg=red,bg=black,bold] #I #[fg=red,nobold]#W #[fg=black,bg=#191724,nobold] '

# Inactive Tab
set -g window-status-format '#[fg=#191724,bg=black] #[fg=brightblack,bg=black] #I #W #[fg=brightblack,bg=black]#[fg=black,bg=#191724,nobold] '

# Activity Tab
set-window-option -ga window-status-activity-style fg=blue,bg=white

# Separator
set -g window-status-separator ''
