(import-macros {: use : after-loaded : setup!} :core.macros)

(fn setup []
  (use lewis6991/gitsigns.nvim (after-loaded mod.git.gitsigns))
  (use tpope/vim-fugitive (after-loaded mod.git.vim-fugitive)))

{: setup}
