(import-macros {: setup!} :core.macros)

(local ts-context (require :ts_context_commentstring.internal))

(fn setup []
  (setup! mini.comment {:hooks {:pre (fn [] (ts-context.update_commentstring))}}))

{: setup}
