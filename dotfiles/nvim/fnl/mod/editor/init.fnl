(import-macros {: use} :core.macros)

(fn setup []
  (use ii14/autosplit.nvim)
  (use tpope/vim-eunuch)
  (use tpope/vim-repeat)
  (use linty-org/readline.nvim :mod :editor.readline)
  ;; (use "$HOME/workspace/son-of-harpoon.git/main" :as :harpoon :mod :editor.harpoon)
  (use echasnovski/mini.comment :after [:treesitter :context-comment] :mod :editor.comment)
  (use echasnovski/mini.ai :init :mini.ai)
  (use echasnovski/mini.surround :init :mini.surround)
  (use echasnovski/mini.pairs :init :mini.pairs)
  (use echasnovski/mini.bufremove :init :mini.bufremove)
  (use echasnovski/mini.align :init :mini.align)
  (use NvChad/nvim-colorizer.lua :cmd :ColorizerToggle :init :colorizer)
  (use nvim-telescope/telescope-fzf-native.nvim :run "make")
  (use nvim-telescope/telescope-live-grep-args.nvim)
  (use nvim-telescope/telescope.nvim :mod :editor.telescope))

{: setup}
