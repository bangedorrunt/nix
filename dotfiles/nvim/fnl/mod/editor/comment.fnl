(import-macros {: setup!} :core.macros)
(local {: create_pre_hook} (require :ts_context_commentstring.integrations.comment_nvim))

(fn setup []
  (setup! Comment {:pre_hooks (create_pre_hook)}))

{: setup}
