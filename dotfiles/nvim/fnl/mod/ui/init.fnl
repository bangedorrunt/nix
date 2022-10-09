(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use catppuccin/nvim :as :catppuccin :run :CatppuccinCompile (after-load mod.ui.catppuccin))
  (use kyazdani42/nvim-web-devicons :module :nvim-web-devicons (after-load mod.ui.devicons))
  (use akinsho/bufferline.nvim :after :catppuccin (after-load mod.ui.bufferline))
  (use nvim-lualine/lualine.nvim :after :catppuccin (after-load mod.ui.lualine))
  (use kyazdani42/nvim-tree.lua :after :catppuccin (after-load mod.ui.nvim-tree)))

{: setup}
