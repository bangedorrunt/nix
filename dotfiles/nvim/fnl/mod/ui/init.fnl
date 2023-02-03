(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use catppuccin/nvim :lazy false :mod :ui.catppuccin)
  (use nvim-tree/nvim-web-devicons :mod :ui.devicons)
  (use nvim-lualine/lualine.nvim :event "User LazyVimStarted" :mod :ui.lualine)
  (use glepnir/dashboard-nvim
       :event :VimEnter
       :opts {:theme :hyper}
       :dependencies :nvim-tree/nvim-web-devicons)
  (use nvim-tree/nvim-tree.lua
       :event "VeryLazy"
       :mod :ui.nvim-tree
       :dependencies :nvim-tree/nvim-web-devicons)
  (use folke/which-key.nvim :event "VeryLazy" :mod :ui.which-key)
  (use folke/todo-comments.nvim :event "BufReadPost" :mod :ui.todo-comments)
  (use folke/noice.nvim
       :event "VeryLazy"
       :mod :ui.noice
       :dependencies [:MunifTanjim/nui.nvim])
  (use echasnovski/mini.tabline
       :version false
       :event "User LazyVimStarted"
       :mod+ :mini.tabline
       :dependencies :nvim-tree/nvim-web-devicons)
  (use echasnovski/mini.indentscope :version false :event :VeryLazy :mod :ui.indentscope)
  (use echasnovski/mini.animate :version false :event :VeryLazy :mod+ :mini.animate)
  ;; HACK statusline
  (require :mod.ui.status)
  )

{: setup}
