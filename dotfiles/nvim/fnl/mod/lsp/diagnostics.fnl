(fn setup []
  (let [{: sign_define} vim.fn
        {: config : severity} vim.diagnostic]
    (config {:underline {:severity {:min severity.INFO}}
             :signs {:severity {:min severity.INFO}}
             :virtual_text false
             :update_in_insert true
             :severity_sort true
             :float {:show_header false :border store.border}})

    (sign_define :DiagnosticSignError {:text store.signs.error :texthl :DiagnosticSignError})
    (sign_define :DiagnosticSignWarn {:text store.signs.warning :texthl :DiagnosticSignWarn})
    (sign_define :DiagnosticSignInfo {:text store.signs.info :texthl :DiagnosticSignInfo})
    (sign_define :DiagnosticSignHint {:text store.signs.hint :texthl :DiagnosticSignHint})))

{: setup}
