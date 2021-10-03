(module plugins.highlight-current-n
  {autoload {nvim aniseed.nvim}
   require-macros [core.macros]})

(nmap [n] :n "<Plug>(highlight-current-n-n)")
(nmap [n] :N "<Plug>(highlight-current-n-N)")
