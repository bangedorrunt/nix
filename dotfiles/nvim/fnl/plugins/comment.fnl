(import-macros {: lazyreq} :core.macros)

(local {: setup} (lazyreq :Comment))
(local ts_context (lazyreq :ts_context_commentstring.integrations.comment_nvim))

(setup {:pre_hook (ts_context.create_pre_hook)})
