(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use catppuccin/nvim :start true :init+ :ui.catppuccin)
  (use nvim-tree/nvim-web-devicons :start true :init+ :ui.devicons)
  (use nvim-lualine/lualine.nvim :event :UIEnter :init+ :ui.lualine)
  (use nvim-tree/nvim-tree.lua :event "User PackerDefered" :init+ :ui.nvim-tree)
  (use echasnovski/mini.tabline :event :UIEnter :init :mini.tabline)
  (use echasnovski/mini.indentscope :event :BufReadPost :init+ :ui.indentscope))

{: setup}
