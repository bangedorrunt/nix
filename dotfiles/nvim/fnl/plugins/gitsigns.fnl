(import-macros {: nmap : noremap} :core.macros)

(let [{: setup} (require :gitsigns)
      {: stage_buffer
       : undo_stage_hunk
       : reset_buffer
       : preview_hunk
       : blame_line
       : toggle_current_line_blame
       : diffthis
       : toggle_deleted} package.loaded.gitsigns
      on_attach
      (fn [bufnr]
        (noremap n expr "]c" '(if (vim.opt.diff:get) "]c" "<Cmd>Gitsigns next_hunk<CR>"))
        (noremap n expr "[c" '(if (vim.opt.diff:get) "[c" "<Cmd>Gitsigns prev_hunk<CR>"))
        (noremap nv "<LocalLeader>hs" "<Cmd>Gitsigns stage_hunk<CR>")
        (noremap nv "<LocalLeader>hr" "<Cmd>Gitsigns reset_hunk<CR>")
        (noremap n  "<LocalLeader>hS" stage_buffer)
        (noremap n  "<LocalLeader>hu" undo_stage_hunk)
        (noremap n  "<LocalLeader>hR" reset_buffer)
        (noremap n  "<LocalLeader>hp" preview_hunk)
        (noremap n  "<LocalLeader>hb" '(blame_line {:full true}))
        (noremap n  "<LocalLeader>tb" toggle_current_line_blame)
        (noremap n  "<LocalLeader>hd" diffthis)
        (noremap n  "<LocalLeader>hD" '(diffthis "~"))
        (noremap n  "<LocalLeader>td" toggle_deleted)
        (noremap ox "ih" ":<C-u>Gitsigns select_hunk<CR>"))]
  (setup {: on_attach
          :signs {:add          {:text "+"}
                  :change       {:text "~"}
                  :delete       {:text "_"}
                  :topdelete    {:text "‾"}
                  :changedelete {:text "~"}}
          :preview_config {:border :solid
                           :style :minimal
                           :relative :cursor}
          :numhl true
          :linehl false
          :watch_gitdir {:interval 1000}
          :current_line_blame false
          :sign_priority 6
          :update_debounce 100
          :status_formatter nil}))
