(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use lewis6991/gitsigns.nvim :event :BufReadPre :mod :git.gitsigns)
  (use tpope/vim-fugitive :event "VeryLazy" :mod :git.vim-fugitive))

{: setup}
