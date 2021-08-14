(module plugins.vim-fugitive 
  {require-macros [core.macros]})


(noremap! [n] "<Leader>gs" "<Cmd>Git<CR>")
(noremap! [n] "<Leader>gc" "<Cmd>Git commit<CR>")
(noremap! [n] "<Leader>gB" "<Cmd>Git blame<CR>")
(noremap! [n] "<Leader>gl" "<Cmd>Gclog<CR>")
(noremap! [n] "<Leader>gf" "<Cmd>Git fetch")
(noremap! [n] "<Leader>gp" "<Cmd>Git pull<CR>")
(noremap! [n] "<Leader>gP" "<Cmd>Git push<CR>")
(noremap! [n] "<Leader>gb" "<Cmd>GBrowse<CR>")
(noremap! [n] "<Leader>gm" ":<C-u>GMove ")
(noremap! [n] "<Leader>gr" ":<C-u>GRename ")
(noremap! [n] "<Leader>gD" "<Cmd>GDelete<CR>")
(noremap! [n] "<Leader>gd" "<Cmd>GRemove<CR>")
