(import-macros {: lazyreq : g} :core.macros)

(g catppuccin_flavour :latte)

(local catppuccin (lazyreq :catppuccin))

(fn setup []
  (catppuccin.setup {:transparent_background true})
  (vim.cmd.colorscheme :catppuccin))

{: setup}
