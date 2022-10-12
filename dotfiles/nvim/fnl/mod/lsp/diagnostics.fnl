(fn setup []
  (let [{: sign_define} vim.fn
        {: config : severity} vim.diagnostic
        signs store.signs
        border store.border]
    (config {:underline {:severity {:min severity.INFO}}
             :signs {:severity {:min severity.INFO}}
             :virtual_text false
             :update_in_insert true
             :severity_sort true
             :float {:show_header false : border}})

    (sign_define :DiagnosticSignError
                 {:text signs.error :texthl :DiagnosticSignError})

    (sign_define :DiagnosticSignWarn
                 {:text signs.warning :texthl :DiagnosticSignWarn})

    (sign_define :DiagnosticSignInfo {:text signs.info :texthl :DiagnosticSignInfo})
    (sign_define :DiagnosticSignHint {:text signs.hint :texthl :DiagnosticSignHint})))

{: setup}
