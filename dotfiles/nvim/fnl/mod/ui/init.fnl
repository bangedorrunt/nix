(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use catppuccin/nvim :lazy false :mod :ui.catppuccin)
  (use nvim-tree/nvim-web-devicons :lazy false :mod :ui.devicons)
  (use nvim-lualine/lualine.nvim :event "User LazyVimStarted" :mod :ui.lualine)
  (use nvim-tree/nvim-tree.lua :event "VeryLazy" :mod :ui.nvim-tree)
  (use echasnovski/mini.tabline :event "User LazyVimStarted" :mod+ :mini.tabline)
  (use echasnovski/mini.indentscope :event :VeryLazy :mod :ui.indentscope))

{: setup}
