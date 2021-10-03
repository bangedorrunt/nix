(module plugins.vim-easy-align
  {autoload {nvim aniseed.nvim}
   require-macros [core.macros]})

(noremap [nv] "<Leader>xa" "<Plug>(EasyAlign)")
