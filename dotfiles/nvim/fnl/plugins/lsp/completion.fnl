(module plugins.lsp.completion)

(defn setup [client buffer]
  (vim.api.nvim_buf_set_option buffer :omnifunc "v:lua.vim.lsp.omnifunc")
  (when (not client.request_orig)
    (set client.request_orig client.request)
    (set client.request
         (fn [method params handler bufnr]
           (when (= method :textDocument/completion)
             (fn intercept [err _method result client-id _bufnr]
               (let [response (or result {})
                     items (or response.items response)]
                 (each [_ item (ipairs items)]
                   (when (and (= item.insertText nil)
                              (or (or (= item.kind 2) (= item.kind 3))
                                  (= item.kind 4)))
                     (set item.insertText (.. item.label "(${1})"))
                     (set item.insertTextFormat 2)))
                 (handler err method result client-id bufnr)))

             (let [___antifnl_rtn_1___ (client.request_orig method params
                                                            intercept bufnr)]
               (lua "return ___antifnl_rtn_1___")))
           (client.request_orig method params handler bufnr)))))
