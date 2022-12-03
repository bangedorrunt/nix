(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use ii14/autosplit.nvim :event "User PackerDefered")
  (use tpope/vim-eunuch :event "User PackerDefered")
  (use tpope/vim-repeat :event "User PackerDefered")
  (use linty-org/readline.nvim :event "User PackerDefered" :init+ :editor.readline)
  (use nvim-treesitter/nvim-treesitter :start true :init+ :editor.treesitter)
  (use p00f/nvim-ts-rainbow :event "User PackerDefered")
  (use andymass/vim-matchup :event "User PackerDefered")
  (use JoosepAlviste/nvim-ts-context-commentstring :config #(vim.cmd "do User CommentStringLoaded") :event "User PackerDefered")
  (use echasnovski/mini.ai :event "User PackerDefered" :init :mini.ai)
  (use echasnovski/mini.surround :event "User PackerDefered" :init :mini.surround)
  (use echasnovski/mini.pairs :event "User PackerDefered" :init :mini.pairs)
  (use echasnovski/mini.bufremove :event "User PackerDefered" :init :mini.bufremove)
  (use echasnovski/mini.align :event "User PackerDefered" :init :mini.align)
  (use echasnovski/mini.comment :event "User CommentStringLoaded" :init+ :editor.comment)
  (use NvChad/nvim-colorizer.lua :event :BufReadPre :init :colorizer)
  (use nvim-telescope/telescope-fzf-native.nvim :event "User TelescopeLoaded" :run "make")
  (use nvim-telescope/telescope-live-grep-args.nvim :event "User TelescopeLoaded")
  (use nvim-telescope/telescope.nvim
       :event ["User PackerDefered" "User NeorgLoaded"]
       :init+ :editor.telescope))

{: setup}
