(import-macros {: augroup : autocmd : autocmd!
                : noremap : command
                : lazyfunc : lazyreq} :core.macros)

;;;; LSP UI
;; Override configuration for floating windows
(let [{: with : handlers} vim.lsp
      border tdt.border
      open_floating_preview vim.lsp.util.open_floating_preview]
  (set vim.lsp.handlers.textDocument/signatureHelp
       (with handlers.signature_help {: border}))
  (set vim.lsp.handlers.textDocument/hover
       (with handlers.hover {: border}))
  (fn vim.lsp.util.open_floating_preview [...]
    (let [(bufnr winid) (open_floating_preview ...)]
      (vim.api.nvim_win_set_option winid :breakindentopt "")
      (vim.api.nvim_win_set_option winid :showbreak "NONE")
      (values bufnr winid))))

(let [{: sign_define} vim.fn
      {: config : severity} vim.diagnostic
      border tdt.border
      signs tdt.signs]
  (config {:underline {:severity {:min severity.INFO}}
           :signs {:severity {:min severity.INFO}}
           :virtual_text false
           :update_in_insert true
           :severity_sort true
           :float {:show_header false : border}})
  (sign_define :DiagnosticSignError {:text signs.error :texthl :DiagnosticSignError})
  (sign_define :DiagnosticSignWarn {:text signs.warning :texthl :DiagnosticSignWarn})
  (sign_define :DiagnosticSignInfo {:text signs.info :texthl :DiagnosticSignInfo})
  (sign_define :DiagnosticSignHint {:text signs.hint :texthl :DiagnosticSignHint}))

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
        {: hover
         : declaration
         : definition
         : implementation
         : type_definition
         : code_action
         : references
         : rename} vim.lsp.buf
        {: open_float
         : goto_prev
         : goto_next} vim.diagnostic]
    ;; LSP keymap
    (noremap n buffer :K hover)
    (noremap nv buffer :gr rename)
    (noremap n buffer "[d" goto_prev)
    (noremap n buffer "]d" goto_next)
    (noremap n buffer :gD declaration)
    (noremap n buffer :gd definition)
    (noremap n buffer :gt type_definition)
    (noremap n buffer :gi implementation)
    (noremap nv buffer :<LocalLeader>la code_action)
    (noremap n buffer :<LocalLeader>ll open_float)
    (noremap n buffer :<LocalLeader>lr references)
    ;; LSP format
    (if (capable? client :documentFormattingProvider)
      (do
        (augroup LspFormatOnSave
                 (autocmd! {:buffer bufnr})
                 (autocmd BufWritePre <buffer>
                          `(vim.lsp.buf.format {:filter
                                                (fn [client] (not (has? [:jsonls
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
               ;; formatting.fnlfmt
               formatting.trim_whitespace
               formatting.shfmt]]

  (-> {: on_attach
       : sources}
      null_ls.setup))

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
