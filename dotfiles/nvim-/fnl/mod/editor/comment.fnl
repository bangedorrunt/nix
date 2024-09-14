(import-macros {: setup!} :core.macros)
(local ts-context (require :ts_context_commentstring))

(fn setup []
  (setup! mini.comment
          {:options {:custom_commentstring #(or (ts-context.calculate_commentstring)
                                                (vim.bo.commentstring))}}))

{: setup}

