(import-macros {: use : after-loaded : setup!} :core.macros)

(fn setup []
  (use ii14/autosplit.nvim)
  (use tpope/vim-eunuch)
  (use tpope/vim-repeat)
  (use linty-org/readline.nvim (after-loaded mod.editor.readline))
  (use kylechui/nvim-surround (after-loaded nvim-surround))
  (use NvChad/nvim-colorizer.lua :cmd :ColorizerToggle (after-loaded colorizer))
  (use "~/workspace/son-of-harpoon.git/main" :as :harpoon (after-loaded mod.editor.harpoon))
  (use nvim-telescope/telescope-fzf-native.nvim :run "make")
  (use nvim-telescope/telescope.nvim (after-loaded mod.editor.telescope)))

{: setup}
