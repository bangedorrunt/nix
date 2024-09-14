(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use lewis6991/gitsigns.nvim :event :BufReadPre :mod :git.gitsigns)
  (use sindrets/diffview.nvim :cmd ["DiffviewOpen" "DiffviewClose" "DiffviewToggleFiles" "DiffviewFocusFiles"])
  (use tpope/vim-fugitive :event "VeryLazy" :mod :git.vim-fugitive))

{: setup}
