(import-macros {: noremap : setup!} :core.macros)

(fn on_attach [buffer]
  (let [{: nav_hunk
         : stage_buffer
         : undo_stage_hunk
         : stage_hunk
         : reset_hunk
         : reset_buffer
         : preview_hunk
         : blame_line
         : toggle_current_line_blame
         : diffthis
         : toggle_deleted} package.loaded.gitsigns]
    ;; FIXME: cursor position outside buffer error
    (noremap n buffer :next-hunk "]h"
             `(if vim.wo.diff
                  (vim.cmd.normal {1 "]c" :bang true})
                  (nav_hunk :next)))
    (noremap n buffer :prev-hunk "[h"
             `(if vim.wo.diff
                  (vim.cmd.normal {1 "[c" :bang true})
                  (nav_hunk :prev)))
    (noremap nv nowait :stage-hunk :<Leader>ghs stage_hunk)
    (noremap nv nowait :reset-hunk :<Leader>ghr reset_hunk)
    (noremap n nowait :stage-buffer :<Leader>ghS stage_buffer)
    (noremap n nowait :undo-stage :<Leader>ghu undo_stage_hunk)
    (noremap n nowait :reset-buffer :<Leader>ghR reset_buffer)
    (noremap n nowait :preview-hunk :<Leader>ghp preview_hunk)
    (noremap n nowait :blame-line :<Leader>ghb `(blame_line {:full true}))
    (noremap n nowait :toggle-blame-line :<Leader>gtb toggle_current_line_blame)
    (noremap n nowait :diff-this :<Leader>ghd diffthis)
    (noremap n nowait "diff-this~" :<Leader>ghD `(diffthis "~"))
    (noremap n nowait :toggle-deleted :<Leader>gtd toggle_deleted)
    (noremap ox :ih ":<C-u>Gitsigns select_hunk<CR>")))

(fn setup []
  (setup! gitsigns {: on_attach
                    :preview_config {:border :solid
                                     :style :minimal
                                     :relative :cursor}
                    :numhl true}))

{: setup}

