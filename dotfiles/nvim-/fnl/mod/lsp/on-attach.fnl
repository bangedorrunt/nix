(import-macros {: augroup
                : autocmd
                : autocmd!
                : noremap} :core.macros)

(local {: has?} (require :core.funs))

(fn on-attach [{:buf bufnr :data {: client_id}}]
  (let [client (vim.lsp.get_client_by_id client_id)
        {: hover
         : declaration
         : definition
         : implementation
         : type_definition
         : code_action
         : references
         : rename
         : format
         : document_highlight
         : clear_references} vim.lsp.buf
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
    (noremap nv buffer nowait "code-action" :<Leader>ca code_action)
    (noremap n buffer nowait "diagnostics" :<Leader>cl open_float)
    (noremap n buffer nowait "references" :<Leader>cr references)
    ;; LSP format
    ;; (when client.server_capabilities.documentFormattingProvider
      ;; FIXME error on second formatting
      ;; (augroup lsp-format-on-save
      ;;   (autocmd! {:buffer bufnr})
      ;;   (autocmd BufWritePre <buffer>
      ;;            `(format {:filter #(not (has? [:jsonls :tsserver] $.name))
      ;;                      : bufnr}
      ;;                     {:buffer bufnr})))
      ;; (noremap n buffer nowait "lsp-format" :<Leader>cf `(format {: bufnr})))
    ;; LSP Document Highlight
    (when client.server_capabilities.documentHighlightProvider
      (augroup lsp-document-highlight
        (autocmd! {:buffer bufnr})
        (autocmd CursorHold <buffer> document_highlight)
        (autocmd CursorMoved <buffer> clear_references)))))

(fn setup []
  (augroup lsp-on-attach
    (autocmd!)
    (autocmd LspAttach * on-attach)))
{: setup}
