(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use catppuccin/nvim :lazy false :mod :ui.catppuccin)
  (use nvim-tree/nvim-web-devicons :lazy false :mod :ui.devicons)
  (use nvim-lualine/lualine.nvim :event "User LazyVimStarted" :mod :ui.lualine)
  (use nvim-tree/nvim-tree.lua :event "VeryLazy" :mod :ui.nvim-tree)
  ;; (use folke/which-key.nvim :event "VeryLazy" :mod+ :which-key)
  (use folke/todo-comments.nvim :event "BufReadPost" :mod :ui.todo-comments)
  ;; REVIEW https://github.com/folke/noice.nvim/issues/298
  ;; (use folke/noice.nvim
  ;;      :event "VeryLazy"
  ;;      :mod :ui.noice
  ;;      :dependencies [:MunifTanjim/nui.nvim])
  (use echasnovski/mini.tabline :event "User LazyVimStarted" :mod+ :mini.tabline)
  (use echasnovski/mini.indentscope :event :VeryLazy :mod :ui.indentscope)
  )

{: setup}
