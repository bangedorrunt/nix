(module plugins.glow
  {autoload {nvim aniseed.nvim}
   require-macros [core.macros]})

(noremap [n] :<Leader>tp "<Cmd>Glow<CR>")
