(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim :ft :fennel)
  (use folke/neodev.nvim)
  (use simrat39/rust-tools.nvim)
  (use meanderingprogrammer/render-markdown.nvim :ft [:Avante :markdown] :opts
       {:file_types [:Avante :markdown]})
  (use nvim-neorg/neorg :ft :norg :mod :lang.neorg)
  (use nvim-orgmode/orgmode :ft :org :mod :lang.orgmode)
  (use yetone/avante.nvim

       :lazy false
       :opts {}
       :dependencies
       [
        ;; :stevearc/dressing.nvim
        :nvim-lua/plenary.nvim
        ;; :MunifTanjim/nui.nvim
        ;; :echasnovski/mini.icons
        :meanderingprogrammer/render-markdown.nvim]))

{: setup}

