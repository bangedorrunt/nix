(import-macros {: setup!} :core.macros)

(fn setup []
  (setup! catppuccin
    {:flavour :mocha
     :term_colors true
     :custom_highlights {:NormalFloat {:link :Pmenu}
                         :MiniTablineVisible {:bg :None}
                         :MiniTablineCurrent {:style [:bold :underline] :sp :#f38ba8}
                         :MiniTablineModifiedCurrent {:bg :None :fg :#f38ba8 :style [:bold :underline]}
                         :MiniTablineModifiedVisible {:bg :None :fg :#f38ba8}
                         :MiniTablineModifiedHidden {:bg :None :fg :#f38ba8}
                         :VertSplit {:bg :#1e1e2e :fg :#1e1e2e}
                         :TelescopeNormal {:link :Pmenu}}
     :color_overrides {:mocha {:base :#14141f
                               :surface0 :#181825}}
     :integrations {:noice true
                    :mini true
                    :harpoon true
                    :treesitter_context true
                    :ts_rainbow true}})
  (vim.cmd.colorscheme :catppuccin))

{: setup}
