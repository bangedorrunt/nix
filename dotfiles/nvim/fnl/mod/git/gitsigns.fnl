(import-macros {: noremap : setup!} :core.macros)

(fn on_attach [bufnr]
  (let [{
         : next_hunk
         : prev_hunk
         : stage_buffer
         : undo_stage_hunk
         : stage_hunk
         : reset_hunk
         : reset_buffer
         : preview_hunk
         : blame_line
         : toggle_current_line_blame
         : diffthis
         : toggle_deleted
         } package.loaded.gitsigns]
    (noremap n expr "]c"
             `(if (vim.opt.diff:get)
                "]c"
                (do
                  (vim.schedule #(next_hunk))
                  "<Ignore>")))
    (noremap n expr "[c"
             `(if (vim.opt.diff:get)
                "[c"
                (do
                  (vim.schedule #(prev_hunk))
                  "<Ignore>")))
    (noremap nv nowait :<LocalLeader>hs stage_hunk)
    (noremap nv nowait :<LocalLeader>hr reset_hunk)
    (noremap n nowait :<LocalLeader>hS stage_buffer)
    (noremap n nowait :<LocalLeader>hu undo_stage_hunk)
    (noremap n nowait :<LocalLeader>hR reset_buffer)
    (noremap n nowait :<LocalLeader>hp preview_hunk)
    (noremap n nowait :<LocalLeader>hb `(blame_line {:full true}))
    (noremap n nowait :<LocalLeader>tb toggle_current_line_blame)
    (noremap n nowait :<LocalLeader>hd diffthis)
    (noremap n nowait :<LocalLeader>hD `(diffthis "~"))
    (noremap n nowait :<LocalLeader>td toggle_deleted)
    (noremap ox :ih ":<C-u>Gitsigns select_hunk<CR>")))

(fn setup []
  (setup! gitsigns
    {: on_attach
     :preview_config {:border :solid :style :minimal :relative :cursor}
     :numhl true})
  )

{: setup}
