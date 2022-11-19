(import-macros {: use} :core.macros)

(fn setup []
  (use catppuccin/nvim :as :catppuccin :opt false :init+ :ui.catppuccin)
  (use kyazdani42/nvim-web-devicons :module :nvim-web-devicons :init+ :ui.devicons)
  (use echasnovski/mini.tabline :event :BufReadPre :init :mini.tabline)
  (use nvim-lualine/lualine.nvim :event "User PackerDefered" :init+ :ui.lualine)
  (use kyazdani42/nvim-tree.lua :event "User PackerDefered" :init+ :ui.nvim-tree)
  (use echasnovski/mini.indentscope :event :BufReadPost :init+ :ui.indentscope)
  ;; (use folke/noice.nvim :event :UIEnter :init+ :ui.noice)
  )

{: setup}
