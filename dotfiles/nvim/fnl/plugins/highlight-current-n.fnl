(module plugins.highlight-current-n
   {require-macros [core.macros]})

(map n :n "<Plug>(highlight-current-n-n)")
(map n :N "<Plug>(highlight-current-n-N)")
(map n "*" "*N")
