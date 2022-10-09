(import-macros {: use : after-loaded : setup!} :core.macros)

(fn setup []
  (use catppuccin/nvim :as :catppuccin :run :CatppuccinCompile (after-loaded mod.ui.catppuccin))
  (use kyazdani42/nvim-web-devicons :module :nvim-web-devicons (after-loaded mod.ui.devicons))
  (use akinsho/bufferline.nvim :after :catppuccin (after-loaded mod.ui.bufferline))
  (use nvim-lualine/lualine.nvim :after :catppuccin (after-loaded mod.ui.lualine))
  (use kyazdani42/nvim-tree.lua :after :catppuccin (after-loaded mod.ui.nvim-tree)))

{: setup}
