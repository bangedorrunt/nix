(import-macros {: lazyreq} :core.macros)

(local Comment (lazyreq :Comment))
(local ts-context (lazyreq :ts_context_commentstring.integrations.comment_nvim))

(fn setup []
  (Comment.setup {:pre_hook (ts-context.create_pre_hook)}))

{: setup}
