(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use ericpruitt/tmux.vim :event "VeryLazy")
  (use aserowy/tmux.nvim :event "VeryLazy" :opts {})
  (use jackMort/ChatGPT.nvim :event "VeryLazy" :opts {}
       :dependencies ["MunifTanjim/nui.nvim"
                      "nvim-lua/plenary.nvim"
                      "nvim-telescope/telescope.nvim"])
  )

{: setup}
