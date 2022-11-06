(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use catppuccin/nvim :as :catppuccin (after-load mod.ui.catppuccin))
  (use kyazdani42/nvim-web-devicons :module :nvim-web-devicons (after-load mod.ui.devicons))
  (use echasnovski/mini.tabline :after :catppuccin (after-load mini.tabline))
  (use nvim-lualine/lualine.nvim :after :catppuccin (after-load mod.ui.lualine))
  (use kyazdani42/nvim-tree.lua :after :catppuccin (after-load mod.ui.nvim-tree))
  (use echasnovski/mini.indentscope (after-load mod.ui.indentscope))
  (use folke/noice.nvim (after-load mod.ui.noice)))

{: setup}
