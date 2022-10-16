(import-macros {: augroup
                : autocmd
                : autocmd!
                : noremap
                : setup!
                : lazyfunc
                : lazyreq} :core.macros)

(local {: has?} (lazyfunc :core.funs))

(local cmp-lsp (lazyreq :cmp_nvim_lsp))
(local rust-tools (lazyreq :rust-tools))
(local neodev (lazyreq :neodev))
(local lspconfig (lazyreq :lspconfig))
(local get-server #(. lspconfig $))

(local mason (lazyreq :mason))
(local mason-lspconfig (lazyreq :mason-lspconfig))

;;;; LSP server config
(fn capable? [client capability]
  (. client.server_capabilities capability))

(local capabilities (cmp-lsp.default_capabilities))

(fn on_attach [client bufnr]
  (let [{: hover
         : declaration
         : definition
         : implementation
         : type_definition
         : code_action
         : references
         : rename} vim.lsp.buf
        {: open_float : goto_prev : goto_next} vim.diagnostic]
    ;; LSP keymap
    (noremap n buffer :K hover)
    (noremap nv buffer :gr rename)
    (noremap n buffer "[d" goto_prev)
    (noremap n buffer "]d" goto_next)
    (noremap n buffer :gD declaration)
    (noremap n buffer :gd definition)
    (noremap n buffer :gt type_definition)
    (noremap n buffer :gi implementation)
    (noremap nv buffer nowait :<LocalLeader>la code_action)
    (noremap n buffer nowait :<LocalLeader>ll open_float)
    (noremap n buffer nowait :<LocalLeader>lr references)
    ;; LSP format
    ;; fnlfmt: skip
    (if (capable? client :documentFormattingProvider)
        (do
          (augroup LspFormatOnSave (autocmd! {:buffer bufnr})
                   (autocmd BufWritePre <buffer>
                            `(vim.lsp.buf.format {:filter (fn [client]
                                                            (not (has? [:fennel-ls
                                                                        :jsonls
                                                                        :tsserver]
                                                                       client.name)))
                                                  : bufnr}
                                                 {:buffer bufnr})))
          (noremap n buffer nowait :<LocalLeader>lf
                   `(vim.lsp.buf.format {: bufnr})))
        (print "LSP not support formatting."))))

(fn setup []
  (setup! mod.lsp.ui)
  (setup! mod.lsp.diagnostics)
  (neodev.setup)
  (mason.setup)
  (mason-lspconfig.setup {:ensure_installed store.lsp.servers})
  (mason-lspconfig.setup_handlers {1 (fn [name]
                                       (let [server (get-server name)]
                                         (-> {: on_attach
                                              : capabilities}
                                             (server.setup))))
                                   :rust_analyzer #(rust-tools.setup)}))

{: setup}
