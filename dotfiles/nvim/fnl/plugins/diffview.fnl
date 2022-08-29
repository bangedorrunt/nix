(import-macros {: nmap : noremap} :core.macros)

(noremap n "<LocalLeader>gvv" "<Cmd>DiffviewOpen<CR>")
(noremap n "<LocalLeader>gvc" "<Cmd>DiffviewClose<CR>")
(noremap n "<LocalLeader>gvf" "<Cmd>DiffviewFocusFiles<CR>")
(noremap n "<LocalLeader>gvh" "<Cmd>DiffviewFileHistory %<CR>")
(noremap n "<LocalLeader>gvt" "<Cmd>DiffviewToggleFiles<CR>")
(noremap n "<LocalLeader>gvr" "<Cmd>DiffviewRefresh<CR>")
