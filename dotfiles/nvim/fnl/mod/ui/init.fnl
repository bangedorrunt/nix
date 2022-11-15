(import-macros {: use} :core.macros)

(fn setup []
  (use catppuccin/nvim :as :catppuccin :mod :ui.catppuccin)
  (use kyazdani42/nvim-web-devicons :module :nvim-web-devicons :mod :ui.devicons)
  (use echasnovski/mini.tabline :after :catppuccin :init :mini.tabline)
  (use nvim-lualine/lualine.nvim :after :catppuccin :mod :ui.lualine)
  (use kyazdani42/nvim-tree.lua :after :catppuccin :mod :ui.nvim-tree)
  (use echasnovski/mini.indentscope :mod :ui.indentscope)
  (use folke/noice.nvim :mod :ui.noice))

{: setup}
