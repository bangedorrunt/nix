(import-macros {: use} :core.macros)

(fn setup []
  (use lewis6991/gitsigns.nvim :event :BufReadPre :init+ :git.gitsigns)
  (use tpope/vim-fugitive :event "User PackerDefered" :init+ :git.vim-fugitive))

{: setup}
