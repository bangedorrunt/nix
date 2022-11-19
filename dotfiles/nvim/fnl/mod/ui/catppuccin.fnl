(import-macros {: setup! : colorscheme} :core.macros)

(fn setup []
  (setup! catppuccin
    {:term_colors true
     :dim_inactive {:enabled true}
     :custom_highlights {:TabLineSel {:bg :None}}
     :color_overrides {:mocha {:base :#14141f
                               :surface0 :#181825}}
     :integrations {;; :noice true
                    :mini true
                    :ts_rainbow true}})
  (colorscheme catppuccin))

{: setup}
