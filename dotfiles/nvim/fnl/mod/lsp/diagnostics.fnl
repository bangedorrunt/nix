(fn setup []
  (let [{: sign_define} vim.fn
        {: config : severity} vim.diagnostic]
    (config {:underline true
             :virtual_text {:spacing 4}
             :update_in_insert false
             :severity_sort true})
    (sign_define :DiagnosticSignError {:text store.signs.error :texthl :DiagnosticSignError :numhl ""})
    (sign_define :DiagnosticSignWarn {:text store.signs.warning :texthl :DiagnosticSignWarn :numhl ""})
    (sign_define :DiagnosticSignInfo {:text store.signs.info :texthl :DiagnosticSignInfo :numhl ""})
    (sign_define :DiagnosticSignHint {:text store.signs.hint :texthl :DiagnosticSignHint :numhl ""})))

{: setup}
