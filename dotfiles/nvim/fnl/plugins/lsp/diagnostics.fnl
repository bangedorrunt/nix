(module plugins.lsp.diagnostics
  {require-macros [core.macros]})

(defn setup []
  (tset vim.lsp.handlers :textDocument/publishDiagnostics
        (vim.lsp.with vim.lsp.diagnostic.on_publish_diagnostics
                      {:underline true
                       :update_in_insert false
                       :virtual_text {:spacing 4 :prefix "●"}
                       :severity_sort true}))
  (def- signs {:Error " " :Warning " " :Hint " " :Information " "})
  (each [type icon (pairs signs)]
    (def- hl (.. :LspDiagnosticsSign type))
    (vim.fn.sign_define hl {:text icon :texthl hl :numhl ""}))	
  )
