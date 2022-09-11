(import-macros {: lazyreq} :core.macros)

(let [{: setup} (lazyreq :Comment)
      ts_context (lazyreq :ts_context_commentstring.integrations.comment_nvim)]
  (setup {:pre_hook (ts_context.create_pre_hook)}))
