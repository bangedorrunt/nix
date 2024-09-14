(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use ii14/autosplit.nvim :event "VeryLazy")
  (use tpope/vim-eunuch :event "VeryLazy")
  (use tpope/vim-repeat :event "VeryLazy")
  (use assistcontrol/readline.nvim :event "VeryLazy" :mod :editor.readline)
  (use ggandor/flit.nvim :opts {:labeled_modes :nv})
  (use ggandor/leap.nvim
       :event "VeryLazy"
       :mod :editor.leap
       :dependencies :ggandor/flit.nvim)
  (use gbprod/yanky.nvim
       :event "VeryLazy"
       :mod :editor.yanky
       :dependencies :kkharji/sqlite.lua)
  (use nvim-treesitter/nvim-treesitter
       :lazy false
       :mod :editor.treesitter
       :build ":TSUpdate"
       :dependencies
       [:mrjones2014/nvim-ts-rainbow
        :andymass/vim-matchup
        :JoosepAlviste/nvim-ts-context-commentstring])
  (use echasnovski/mini.bracketed :version false :event "VeryLazy" :opts {})
  (use echasnovski/mini.ai :version false :event "VeryLazy" :opts {})
  (use echasnovski/mini.surround :version false :event "VeryLazy" :opts {})
  ;; (use echasnovski/mini.pairs :version false :event "VeryLazy" :opts {})
  (use echasnovski/mini.bufremove :version false :event "VeryLazy" :opts {})
  (use echasnovski/mini.align :version false :event "VeryLazy" :opts {})
  (use echasnovski/mini.comment
       :version false
       :lazy true
       :mod :editor.comment
       :dependencies :JoosepAlviste/nvim-ts-context-commentstring)
  (use nvchad/nvim-colorizer.lua
       :cmd ["ColorizerToggle"
             "ColorizerAttachToBuffer"
             "ColorizerDeattachFromBuffer"
             "ColorizerReloadAllBuffers"]
       :opts)
  (use joosepalviste/nvim-ts-context-commentstring :event "VeryLazy" :opts {:enable_autocmd false})
  (use nvim-telescope/telescope-fzf-native.nvim :build "make")
  (use nvim-telescope/telescope.nvim
       :event "VeryLazy"
       :mod :editor.telescope
       :dependencies
       [:nvim-lua/plenary.nvim
        :nvim-telescope/telescope-fzf-native.nvim
        :nvim-telescope/telescope-live-grep-args.nvim
        :nvim-neorg/neorg-telescope]))

{: setup}
