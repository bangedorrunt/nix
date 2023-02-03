(import-macros {: noremap} :core.macros)

(fn setup []
  (noremap n nowait "git-status" :<Leader>gs :<Cmd>Git<CR>)
  (noremap n nowait "git-commit" :<Leader>gc "<Cmd>Git commit<CR>")
  (noremap n nowait "git-blame" :<Leader>gB "<Cmd>Git blame<CR>")
  (noremap n nowait "git-log" :<Leader>gl :<Cmd>Gclog<CR>)
  (noremap n nowait "git-fetch" :<Leader>gf "<Cmd>Git fetch<CR>")
  (noremap n nowait "git-pull" :<Leader>gp "<Cmd>Git pull<CR>")
  (noremap n nowait "git-push" :<Leader>gP "<Cmd>Git push<CR>")
  (noremap n nowait "git-browse" :<Leader>gb :<Cmd>GBrowse<CR>)
  (noremap n nowait "git-move" :<Leader>gm ":<C-u>GMove ")
  (noremap n nowait "git-rename" :<Leader>gr ":<C-u>GRename ")
  (noremap n nowait "git-delete" :<Leader>gD :<Cmd>GDelete<CR>)
  (noremap n nowait "git-remove" :<Leader>gd :<Cmd>GRemove<CR>)
  )

{: setup}
