(import-macros {: setup! : colorscheme} :core.macros)

(fn setup []
  (setup! catppuccin
    {:term_colors true
     :dim_inactive {:enabled true}
     :custom_highlights {:TabLineSel {:bg :None}
                         :DashboardHeader {:fg "#8AFF80"}
                         :DashboardKey {:fg "#FF9580"}
                         :DashboardDesc {:fg "#87FEF8"}
                         :DashboardIcon {:fg "#FCFC7F" :bold true}
                         :DashboardFooter {:fg "#5D5D65"}
                         }
     :color_overrides {:mocha {:base :#14141f
                               :surface0 :#181825}}
     :integrations {:noice true
                    :mini true
                    :which_key true
                    :leap true
                    :treesitter_context true
                    :ts_rainbow true}})
  (colorscheme catppuccin))

{: setup}
