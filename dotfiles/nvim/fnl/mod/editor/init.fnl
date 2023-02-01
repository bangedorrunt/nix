(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use ii14/autosplit.nvim :event "VeryLazy")
  (use tpope/vim-eunuch :event "VeryLazy")
  (use tpope/vim-repeat :event "VeryLazy")
  (use linty-org/readline.nvim :event "VeryLazy" :mod :editor.readline)
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
  (use echasnovski/mini.ai :version false :event "VeryLazy" :mod+ :mini.ai)
  (use echasnovski/mini.surround :version false :event "VeryLazy" :mod+ :mini.surround)
  (use echasnovski/mini.pairs :version false :event "VeryLazy" :mod+ :mini.pairs)
  (use echasnovski/mini.bufremove :version false :event "VeryLazy" :mod+ :mini.bufremove)
  (use echasnovski/mini.align :version false :event "VeryLazy" :mod+ :mini.align)
  (use echasnovski/mini.comment
       :enabled false
       :version false
       :event "VeryLazy"
       :mod :editor.comment
       :dependencies :JoosepAlviste/nvim-ts-context-commentstring)
  (use numToStr/Comment.nvim
       :event "VeryLazy"
       :mod :editor.comment
       :dependencies :JoosepAlviste/nvim-ts-context-commentstring)
  (use NvChad/nvim-colorizer.lua
       :cmd ["ColorizerToggle"
             "ColorizerAttachToBuffer"
             "ColorizerDeattachFromBuffer"
             "ColorizerReloadAllBuffers"]
       :mod+ :colorizer)
  (use :nvim-telescope/telescope-fzf-native.nvim :build "make")
  (use nvim-telescope/telescope.nvim
       :event "VeryLazy"
       :mod :editor.telescope
       :dependencies
       [:nvim-telescope/telescope-fzf-native.nvim
        :nvim-telescope/telescope-live-grep-args.nvim
        :nvim-neorg/neorg-telescope]))

{: setup}
