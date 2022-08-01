(module plugins.lsp
  {autoload {{: contains?} core.utils
             lsp-config lspconfig
             : mason
             : mason-lspconfig
             lsp-signature lsp_signature
             : null-ls
             null-ls-builtins null-ls.builtins}
   require-macros [core.macros]})

;;;; LSP UI
(let [{: with : handlers} vim.lsp]
  (set vim.lsp.handlers.textDocument/signatureHelp
       (with handlers.signature_help {:border :solid}))
  (set vim.lsp.handlers.textDocument/hover
       (with handlers.hover {:border :solid})))

;; More general LSP commands
(defn- reload-lsp []
  (vim.lsp.stop_client (vim.lsp.get_active_clients))
  (vim.cmd :edit))

(defn- open-lsp-log []
  (let [path (vim.lsp.get_log_path)]
    (vim.cmd (string.format "edit %s" path))))

(command LspLog '(open-lsp-log))
(command LspRestart '(reload-lsp))

;;;; Diagnostics Configuration
(let [{: config : severity} vim.diagnostic
      {: sign_define} vim.fn]
  (config {:underline {:severity {:min severity.INFO}}
           :signs {:severity {:min severity.INFO}}
           :virtual_text false ;; lsp_lines handles this
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

(defn- enhanced-attach [client bufnr]
  (let [{:hover open-doc-float!
         :declaration goto-declaration!
         :definition goto-definition!
         :type_definition goto-type-definition!
         :code_action open-code-action-float!
         :rename rename!} vim.lsp.buf
        {:open_float open-line-diag-float!
         :goto_prev goto-diag-prev!
         :goto_next goto-diag-next!} vim.diagnostic
        {:lsp_implementations open-impl-float!
         :lsp_references open-ref-float!
         :diagnostics open-diag-float!
         :lsp_document_symbols open-local-symbol-float!
         :lsp_workspace_symbols open-workspace-symbol-float!} (require :telescope.builtin)]
    ;; LSP keymap
    (noremap n buffer "K" open-doc-float!)
    (noremap nv buffer "<leader>la" open-code-action-float!)
    (noremap nv buffer "<leader>lr" rename!)
    (noremap n buffer "<leader>ll" open-line-diag-float!)
    (noremap n buffer "[d" goto-diag-prev!)
    (noremap n buffer "]d" goto-diag-next!)
    (noremap n buffer "gD" goto-declaration!)
    (noremap n buffer "gd" goto-definition!)
    (noremap n buffer "gt" goto-type-definition!)
    (noremap n buffer "<leader>li" open-impl-float!)
    (noremap n buffer "<leader>ly" open-ref-float!)
    (noremap n buffer "<leader>ld" '(open-diag-float! {:bufnr 0}))
    (noremap n buffer "<leader>lD" open-diag-float!)
    (noremap n buffer "<leader>ls" open-local-symbol-float!)
    (noremap n buffer "<leader>lS" open-workspace-symbol-float!)
    ;; LSP format
    (if (capable? client :documentFormattingProvider)
      (do
        (augroup LspFormatOnSave
                 (autocmd! * <buffer>)
                 (autocmd BufWritePre <buffer>
                          '(vim.lsp.buf.format {:filter (fn [client]
                                                          (not (contains? [:jsonls :tsserver] client.name)))
                                                :bufnr bufnr} {:buffer bufnr})))
        (noremap n buffer :<Leader>lf '(vim.lsp.buf.format {:bufnr bufnr})))
      (print "LSP not support formatting."))))

;; fnlfmt: skip
(def- servers [:bashls
               :clojure_lsp
               :cssls
               :diagnosticls
               :dockerls
               :emmet_ls
               :eslint
               :html
               :jsonls
               :sumneko_lua
               :tailwindcss
               :tsserver
               :vimls
               :yamlls])

(mason.setup)
(mason-lspconfig.setup {:ensure_installed servers})
(mason-lspconfig.setup_handlers
  [(fn [server]
     (let [lsp-installed-server (. lsp-config server)]
       (-> {:on_attach enhanced-attach
            : capabilities
            :flags {:debounce_text_changes 150}}
           (lsp-installed-server.setup))))])

;; fnlfmt: skip
;; WARNING: when you experience any lag or unresponsive with Lsp,
;; make sure respective sources are installed
;; In my case:
;; Typescript was slow because `eslint_d` was not installed
;; Markdown was slow because `write-good` and `markdownlint`
;; was not installed
(-> {:debounce 150
     :sources ((fn []
                 [null-ls-builtins.formatting.prettier
                  null-ls-builtins.formatting.stylua
                  ; null-ls-builtins.formatting.fnlfmt
                  null-ls-builtins.formatting.trim_whitespace
                  null-ls-builtins.formatting.shfmt
                  (null-ls-builtins.diagnostics.shellcheck.with {:filetypes [:zsh
                                                                             :sh
                                                                             :bash]})]))
     :on_attach enhanced-attach}
    (null-ls.setup))
