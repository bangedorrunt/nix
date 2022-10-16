(import-macros {: augroup
                : autocmd
                : autocmd!
                : noremap
                : setup!
                : lazyfunc
                : lazyreq} :core.macros)

(local {: has?} (lazyfunc :core.funs))

(fn capable? [client capability]
  (. client.server_capabilities capability))

(fn on-attach [args] (let [bufnr args.buf
        client (vim.lsp.get_client_by_id args.data.client_id)
        {: hover
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
  (augroup lsp-on-attach
           (autocmd!)
           (autocmd LspAttach * on-attach)))
{: setup}
