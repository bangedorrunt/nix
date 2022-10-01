(import-macros {: noremap} :core.macros)

(fn setup []
  (noremap n nowait :<LocalLeader>gs :<Cmd>Git<CR>)
  (noremap n nowait :<LocalLeader>gc "<Cmd>Git commit<CR>")
  (noremap n nowait :<LocalLeader>gB "<Cmd>Git blame<CR>")
  (noremap n nowait :<LocalLeader>gl :<Cmd>Gclog<CR>)
  (noremap n nowait :<LocalLeader>gf "<Cmd>Git fetch<CR>")
  (noremap n nowait :<LocalLeader>gp "<Cmd>Git pull<CR>")
  (noremap n nowait :<LocalLeader>gP "<Cmd>Git push<CR>")
  (noremap n nowait :<LocalLeader>gb :<Cmd>GBrowse<CR>)
  (noremap n nowait :<LocalLeader>gm ":<C-u>GMove ")
  (noremap n nowait :<LocalLeader>gr ":<C-u>GRename ")
  (noremap n nowait :<LocalLeader>gD :<Cmd>GDelete<CR>)
  (noremap n nowait :<LocalLeader>gd :<Cmd>GRemove<CR>))

{: setup}
