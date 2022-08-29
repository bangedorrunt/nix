(let [{: setup} (require :Comment)
      ts_context (require :ts_context_commentstring.integrations.comment_nvim)]
  (setup {:pre_hook (ts_context.create_pre_hook)}))
