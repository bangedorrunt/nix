(import-macros {: lazyreq : after! : setup!} :core.macros)
(local plugins
  [[:lewis6991/gitsigns.nvim]
   [:tpope/vim-fugitive]])

(fn setup []
  (after! :gitsigns.nvim (setup! :mod.git.gitsigns))
  (after! :vim-fugitive (setup! :mod.git.vim-fugitive)))

{: plugins
 : setup}
