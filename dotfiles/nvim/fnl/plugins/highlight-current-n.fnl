(module plugins.highlight-current-n
  {require-macros [core.macros]})

(nmap! [n] :n "<Plug>(highlight-current-n-n)")
(nmap! [n] :N "<Plug>(highlight-current-n-N)")
