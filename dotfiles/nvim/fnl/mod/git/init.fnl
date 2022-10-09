(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use lewis6991/gitsigns.nvim (after-load mod.git.gitsigns))
  (use tpope/vim-fugitive (after-load mod.git.vim-fugitive)))

{: setup}
