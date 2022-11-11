(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use ii14/autosplit.nvim)
  (use tpope/vim-eunuch)
  (use tpope/vim-repeat)
  (use linty-org/readline.nvim (after-load mod.editor.readline))
  (use "$HOME/workspace/son-of-harpoon.git/main" :as :harpoon (after-load mod.editor.harpoon))
  (use echasnovski/mini.comment :after [:treesitter :context-comment] (after-load mod.editor.comment))
  (use echasnovski/mini.ai (after-load mini.ai))
  (use echasnovski/mini.surround (after-load mini.surround))
  (use echasnovski/mini.pairs (after-load mini.pairs))
  (use echasnovski/mini.bufremove (after-load mini.bufremove))
  (use echasnovski/mini.align (after-load mini.align))
  (use NvChad/nvim-colorizer.lua :cmd :ColorizerToggle (after-load colorizer))
  (use nvim-telescope/telescope-fzf-native.nvim :run "make")
  (use nvim-telescope/telescope.nvim (after-load mod.editor.telescope)))

{: setup}
