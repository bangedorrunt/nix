(fn setup []
  (let [{: sign_define} vim.fn
        {: config : severity} vim.diagnostic]
    (config {:underline true
             :virtual_text {:spacing 4}
             :update_in_insert false
             :severity_sort true})
    (sign_define :DiagnosticSignError {:text "" :texthl :DiagnosticSignError :numhl ""})
    (sign_define :DiagnosticSignWarn {:text "" :texthl :DiagnosticSignWarn :numhl ""})
    (sign_define :DiagnosticSignInfo {:text "" :texthl :DiagnosticSignInfo :numhl ""})
    (sign_define :DiagnosticSignHint {:text "" :texthl :DiagnosticSignHint :numhl ""})))

{: setup}
