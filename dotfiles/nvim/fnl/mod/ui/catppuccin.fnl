(import-macros {: setup!} :core.macros)

(fn setup []
  (setup! catppuccin
    {:flavour :mocha
     :term_colors true
     :dim_inactive {:enabled true}
     :custom_highlights {:NormalFloat {:link :Pmenu}
                         :VertSplit {:bg :#14141f :fg :#14141f}
                         :MiniTablineVisible {:bg :None}
                         :MiniTablineCurrent {:underline true :sp :#ff79c6}
                         :MiniTablineModifiedCurrent {:bg :None :fg :#ff79c6 :bold true :underline true}
                         :MiniTablineModifiedVisible {:bg :None :fg :#f38ba8}
                         :MiniTablineModifiedHidden {:bg :None :fg :#f38ba8}}
     :color_overrides {:mocha {:base :#14141f
                               :surface0 :#181825}}
     :integrations {:noice true
                    :mini true
                    :harpoon true
                    :treesitter_context true
                    :ts_rainbow true}})
  (vim.cmd.colorscheme :catppuccin))

{: setup}
