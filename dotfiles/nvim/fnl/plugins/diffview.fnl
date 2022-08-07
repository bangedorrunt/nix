(module plugins.diffview
  {require-macros [core.macros]})

(noremap n "<Leader>gvv" "<Cmd>DiffviewOpen<CR>")
(noremap n "<Leader>gvc" "<Cmd>DiffviewClose<CR>")
(noremap n "<Leader>gvf" "<Cmd>DiffviewFocusFiles<CR>")
(noremap n "<Leader>gvh" "<Cmd>DiffviewFileHistory %<CR>")
(noremap n "<Leader>gvt" "<Cmd>DiffviewToggleFiles<CR>")
(noremap n "<Leader>gvr" "<Cmd>DiffviewRefresh<CR>")
