(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use ii14/autosplit.nvim)
  (use tpope/vim-eunuch)
  (use tpope/vim-repeat)
  (use linty-org/readline.nvim (after-load mod.editor.readline))
  (use kylechui/nvim-surround (after-load nvim-surround))
  (use NvChad/nvim-colorizer.lua :cmd :ColorizerToggle (after-load colorizer))
  (use "~/workspace/son-of-harpoon.git/main" :as :harpoon (after-load mod.editor.harpoon))
  (use nvim-telescope/telescope-fzf-native.nvim :run "make")
  (use nvim-telescope/telescope.nvim (after-load mod.editor.telescope)))

{: setup}
