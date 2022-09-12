(import-macros {: augroup : autocmd : autocmd!
                : noremap : command
                : lazyfunc : lazyreq} :core.macros)
;;;; LSP UI
(let [{: with : handlers} vim.lsp]
  (set vim.lsp.handlers.textDocument/signatureHelp
       (with handlers.signature_help {:border :solid}))
  (set vim.lsp.handlers.textDocument/hover
       (with handlers.hover {:border :solid})))

;; More general LSP commands

;; fnlfmt: skip
(fn reload_lsp []
  (vim.lsp.stop_client (vim.lsp.get_active_clients))
  (vim.cmd.edit))

;; fnlfmt: skip
(fn open_lsp_log []
  (let [path (vim.lsp.get_log_path)]
    (vim.cmd.edit path)))

(command LspLog open_lsp_log)
(command LspRestart reload_lsp)

;;;; Diagnostics Configuration
(let [{: config : severity} vim.diagnostic
      {: sign_define} vim.fn]
  (config {:underline {:severity {:min severity.INFO}}
           :signs {:severity {:min severity.INFO}}
           :virtual_text false
           ;; lsp_lines handles this
           :update_in_insert true
           :severity_sort true
           :float {:show_header false :border :rounded}})
  (sign_define :DiagnosticSignError {:text "" :texthl :DiagnosticSignError})
  (sign_define :DiagnosticSignWarn {:text "" :texthl :DiagnosticSignWarn})
  (sign_define :DiagnosticSignInfo {:text "" :texthl :DiagnosticSignInfo})
  (sign_define :DiagnosticSignHint {:text "" :texthl :DiagnosticSignHint}))

;;;; LSP server config
(fn capable? [client capability] (. client.server_capabilities capability))

(local capabilities
  (let [c (vim.lsp.protocol.make_client_capabilities)]
    ;; NOTE: use `cmp_nvim_lsp.update_capabilities` is unneccessary
    ;; https://github.com/hrsh7th/cmp-nvim-lsp/blob/f6f471898bc4b45eacd36eef9887847b73130e0e/lua/cmp_nvim_lsp/init.lua#L23
    ;; Delegate snippet support to any completion engine such as `nvim-cmp`
    (set c.textDocument.completion.completionItem
         {:documentationFormat [:markdown :plaintext]
          :snippetSupport true
          :preselectSupport true
          :insertReplaceSupport true
          :labelDetailsSupport true
          :deprecatedSupport true
          :commitCharactersSupport true
          :tagSupport {:valueSet {1 1}}
          :resolveSupport {:properties [:documentation
                                        :detail
                                        :additionalTextEdits]}})))

(fn on_attach [client bufnr]
  (let [{: has?} (lazyfunc :core.funs)
        {:hover open_doc_float!
         :declaration goto_declaration!
         :definition goto_definition!
         :implementation goto_implementation!
         :type_definition goto_type_definition!
         :code_action open_code_action_float!
         :references open_references_float!
         :rename rename!} vim.lsp.buf
        {:open_float open_line_diag_float!
         :goto_prev goto_diag_prev!
         :goto_next goto_diag_next!} vim.diagnostic]
    ;; LSP keymap
    (noremap n buffer :K open_doc_float!)
    (noremap nv buffer :gr rename!)
    (noremap n buffer "[d" goto_diag_prev!)
    (noremap n buffer "]d" goto_diag_next!)
    (noremap n buffer :gD goto_declaration!)
    (noremap n buffer :gd goto_definition!)
    (noremap n buffer :gt goto_type_definition!)
    (noremap n buffer :gi goto_implementation!)
    (noremap nv buffer :<LocalLeader>la open_code_action_float!)
    (noremap n buffer :<LocalLeader>ll open_line_diag_float!)
    (noremap n buffer :<LocalLeader>lr open_references_float!)
    ;; LSP format
    (if (capable? client :documentFormattingProvider)
      (do
        (augroup LspFormatOnSave
                 (autocmd! {:buffer bufnr})
                 (autocmd BufWritePre <buffer>
                          `(vim.lsp.buf.format {:filter
                                                (fn [client] (not (has? [:fennel
                                                                         :jsonls
                                                                         :tsserver]
                                                                        client.name)))
                                                : bufnr}
                                               {:buffer bufnr})))
        (noremap n buffer :<LocalLeader>lf `(vim.lsp.buf.format {: bufnr})))
      (print "LSP not support formatting."))))

(let [servers [:bashls
               :clojure_lsp
               :cssls
               :diagnosticls
               :dockerls
               :emmet_ls
               :eslint
               :html
               :jsonls
               :marksman
               :rust_analyzer
               :sumneko_lua
               :tailwindcss
               :tsserver
               :vimls
               :yamlls]
      lspconfig (lazyreq :lspconfig)
      mason (lazyreq :mason)
      mason_lspconfig (lazyreq :mason-lspconfig)]
  (mason.setup)
  (mason_lspconfig.setup {:ensure_installed servers})
  (mason_lspconfig.setup_handlers
    {1 (fn [server]
         (let [lsp_installed_server (. lspconfig server)]
           (-> {: on_attach
                : capabilities}
               (lsp_installed_server.setup))))
     :sumneko_lua #(let [{: sumneko_lua} lspconfig
                         {:setup lua_dev} (lazyreq :lua-dev)]
                     (sumneko_lua.setup (lua_dev)))
     :rust_analyzer #(let [rust_tools (lazyreq :rust-tools)]
                       (rust_tools.setup))}))

;; WARNING: when you experience any lag or unresponsive with Lsp,
;; make sure respective sources are installed
(let [null_ls (lazyreq :null-ls)
      {: formatting
       : diagnostics} (lazyreq :null-ls.builtins)
      sources [formatting.prettier
               formatting.stylua
               formatting.trim_whitespace
               formatting.shfmt]]

  (-> {: on_attach
       : sources}
      null_ls.setup))
