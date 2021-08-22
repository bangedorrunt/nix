(module plugins.vim-easy-align)

(import-macros {: noremap! : nmap!} :core.macros)

(noremap! [nx] "<Leader>xa" "<Plug>(EasyAlign)")
