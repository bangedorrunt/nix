(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use ericpruitt/tmux.vim :event "VeryLazy")
  (use aserowy/tmux.nvim :event "VeryLazy" :mod+ :tmux)
  (use jackMort/ChatGPT.nvim :event "VeryLazy" :mod+ :chatgpt
       :dependencies ["MunifTanjim/nui.nvim"
                      "nvim-lua/plenary.nvim"
                      "nvim-telescope/telescope.nvim"])
  ;; featured in 0.9 nightly
  ;; (use gpanders/editorconfig.nvim)
  )

{: setup}
