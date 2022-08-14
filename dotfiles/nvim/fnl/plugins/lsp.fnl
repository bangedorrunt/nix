
;; fnlfmt: skip
(module plugins.lsp
  {autoload {{: contains?} core.funs
             : lspconfig
             : mason
             mason_lspconfig mason-lspconfig
             rust_tools rust-tools
             null_ls null-ls
             {: formatting : diagnostics} null-ls.builtins
             telescope_builtin telescope.builtin}
   require-macros [core.macros]})

;;;; LSP UI
(let [{: with : handlers} vim.lsp]
  (set vim.lsp.handlers.textDocument/signatureHelp
       (with handlers.signature_help {:border :solid}))
  (set vim.lsp.handlers.textDocument/hover
       (with handlers.hover {:border :solid})))

;; More general LSP commands

;; fnlfmt: skip
(defn- reload-lsp []
  (vim.lsp.stop_client (vim.lsp.get_active_clients))
  (vim.cmd.edit))

;; fnlfmt: skip
(defn- open-lsp-log []
  (let [path (vim.lsp.get_log_path)]
    (vim.cmd.edit path)))

(command LspLog '(open-lsp-log))
(command LspRestart '(reload-lsp))

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
(defn- capable? [client capability] (. client.server_capabilities capability))

(def- capabilities
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

(defn- enhanced_attach [client bufnr]
       (let [{:hover open_doc_float!
              :declaration goto_declaration!
              :definition goto_definition!
              :type_definition goto_type_definition!
              :code_action open_code_action_float!
              :rename rename!} vim.lsp.buf
             {:open_float open_line_diag_float!
              :goto_prev goto_diag_prev!
              :goto_next goto_diag_next!} vim.diagnostic
             {:lsp_implementations open_impl_float!
              :lsp_references open_ref_float!
              :diagnostics open_diag_float!
              :lsp_document_symbols open_local_symbol_float!
              :lsp_workspace_symbols open_workspace_symbol_float!} telescope_builtin]
         ;; LSP keymap
         (noremap n buffer :K open_doc_float!)
         (noremap nv buffer :gr rename!)
         (noremap n buffer "[d" goto_diag_prev!)
         (noremap n buffer "]d" goto_diag_next!)
         (noremap n buffer :gD goto_declaration!)
         (noremap n buffer :gd goto_definition!)
         (noremap n buffer :gt goto_type_definition!)
         (noremap nv buffer :<leader>la open_code_action_float!)
         (noremap n buffer :<leader>ll open_line_diag_float!)
         (noremap n buffer :<leader>li open_impl_float!)
         (noremap n buffer :<leader>lr open_ref_float!)
         (noremap n buffer :<leader>ld `(open_diag_float! {:bufnr 0}))
         (noremap n buffer :<leader>lD open_diag_float!)
         (noremap n buffer :<leader>ls open_local_symbol_float!)
         (noremap n buffer :<leader>lS open_workspace_symbol_float!)
         ;; LSP format
         (if (capable? client :documentFormattingProvider)
             (do
               (augroup LspFormatOnSave (autocmd! * <buffer>)
                        (autocmd BufWritePre <buffer>
                                 `(vim.lsp.buf.format {:filter #(not (contains? [:jsonls
                                                                                 :tsserver]
                                                                                $.name))
                                                       : bufnr}
                                                      {:buffer bufnr})))
               (noremap n buffer :<Leader>lf `(vim.lsp.buf.format {: bufnr})))
             (print "LSP not support formatting."))))

(def- servers [:bashls
               :clojure_lsp
               :cssls
               :diagnosticls
               :dockerls
               :emmet_ls
               :eslint
               :html
               :jsonls
               :rust_analyzer
               :sumneko_lua
               :tailwindcss
               :tsserver
               :vimls
               :yamlls])

(mason.setup)
(mason_lspconfig.setup {:ensure_installed servers})
(mason_lspconfig.setup_handlers
  {1 (fn [server]
       (let [lsp_installed_server (. lspconfig
                                     server)]
         (-> {:on_attach enhanced_attach
              : capabilities}
             (lsp_installed_server.setup))))
   :rust_analyzer #(rust_tools.setup)})

;; WARNING: when you experience any lag or unresponsive with Lsp,
;; make sure respective sources are installed
(def- null_sources
      [formatting.prettier
       formatting.stylua
       ;; formatting.fnlfmt
       formatting.trim_whitespace
       formatting.shfmt
       (diagnostics.shellcheck.with {:filetypes [:zsh
                                                 :sh
                                                 :bash]})])

(-> {:on_attach enhanced_attach :sources null_sources}
    null_ls.setup)
