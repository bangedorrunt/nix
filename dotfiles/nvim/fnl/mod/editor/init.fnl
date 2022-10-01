(import-macros {: lazyreq : after! : before! : setup! : g} :core.macros)

(local plugins
  [[:ii14/autosplit.nvim]
   [:linty-org/readline.nvim]
   [:tpope/vim-eunuch]
   [:tpope/vim-repeat]
   [:kylechui/nvim-surround]
   [:NvChad/nvim-colorizer.lua]
   ["~/workspace/son-of-harpoon.git/main" :as :harpoon]
   [:nvim-telescope/telescope-fzf-native.nvim :run "make"]
   [:nvim-telescope/telescope.nvim]])

(fn setup []
  (before! :vim-eunuch (g eunuch_no_maps true))
  (after! :readline.nvim (setup! :mod.editor.readline))
  (after! :nvim-surround (setup! :nvim-surround))
  (after! :nvim-colorizer.lua (setup! :colorizer))
  (after! :harpoon (setup! :mod.editor.harpoon))
  (after! :telescope.nvim (setup! :mod.editor.telescope)))

{: plugins
 : setup}
