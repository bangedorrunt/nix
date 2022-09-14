(import-macros {: augroup
                : autocmd
                : autocmd!
                : noremap
                : command
                : lazyfunc
                : lazyreq} :core.macros)

(local {: has? : concat} (lazyfunc :core.funs))

(local rust-tools (lazyreq :rust-tools))
(local lspconfig (lazyreq :lspconfig))
(local server-configs (lazyreq :lspconfig.configs))

(local mason (lazyreq :mason))
(local mason-lspconfig (lazyreq :mason-lspconfig))

(local get-server #(. lspconfig $))
(local sumneko-lua (get-server :sumneko_lua))
(local {:setup lua-dev} (lazyreq :lua-dev))

(local null-ls (lazyreq :null-ls))
(local {: formatting : diagnostics} (lazyreq :null-ls.builtins))

(local signs tdt.signs)
(local border tdt.border)

;;;; LSP UI
;; Override configuration for floating windows
(local {: with : handlers} vim.lsp)
(local open-floating-preview vim.lsp.util.open_floating_preview)
(set vim.lsp.handlers.textDocument/signatureHelp
     (with handlers.signature_help {: border}))

(set vim.lsp.handlers.textDocument/hover (with handlers.hover {: border}))

(fn vim.lsp.util.open_floating_preview [...]
  (let [(bufnr winid) (open-floating-preview ...)]
    (vim.api.nvim_win_set_option winid :breakindentopt "")
    (vim.api.nvim_win_set_option winid :showbreak :NONE)
    (values bufnr winid)))

(local {: sign_define} vim.fn)
(local {: config : severity} vim.diagnostic)
(config {:underline {:severity {:min severity.INFO}}
         :signs {:severity {:min severity.INFO}}
         :virtual_text false
         :update_in_insert true
         :severity_sort true
         :float {:show_header false : border}})

(sign_define :DiagnosticSignError
             {:text signs.error :texthl :DiagnosticSignError})

(sign_define :DiagnosticSignWarn
             {:text signs.warning :texthl :DiagnosticSignWarn})

(sign_define :DiagnosticSignInfo {:text signs.info :texthl :DiagnosticSignInfo})
(sign_define :DiagnosticSignHint {:text signs.hint :texthl :DiagnosticSignHint})

;;;; LSP server config
(fn capable? [client capability]
  (. client.server_capabilities capability))

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
    (noremap nv buffer :<LocalLeader>la code_action)
    (noremap n buffer :<LocalLeader>ll open_float)
    (noremap n buffer :<LocalLeader>lr references)
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
          (noremap n buffer :<LocalLeader>lf `(vim.lsp.buf.format {: bufnr})))
        (print "LSP not support formatting."))))

(local servers [:bashls
                :clojure_lsp
                :cssls
                :diagnosticls
                :dockerls
                :emmet_ls
                :eslint
                ;; :fennel-ls
                :html
                :jsonls
                :marksman
                :rust_analyzer
                :sumneko_lua
                :tailwindcss
                :tsserver
                :vimls
                :yamlls])

(mason.setup)
(mason-lspconfig.setup {:ensure_installed servers})
(mason-lspconfig.setup_handlers {1 (fn [name]
                                     (let [server (get-server name)]
                                       (-> {: on_attach : capabilities}
                                           (server.setup))))
                                 :sumneko_lua #(sumneko-lua.setup (lua-dev))
                                 :rust_analyzer #(rust-tools.setup)})

;; WARNING: when you experience any lag or unresponsive with Lsp,
;; make sure respective sources are installed
(local null-sources [formatting.prettier
                     formatting.stylua
                     formatting.fnlfmt
                     formatting.trim_whitespace
                     formatting.shfmt])

(-> {: on_attach :sources null-sources}
    null-ls.setup)

(tset server-configs :fennel-ls
      {:default_config {:cmd [(concat (vim.fn.stdpath :data)
                                      :/mason/bin/fennel-ls)]
                        :filetypes [:fennel]
                        :root_dir (fn [dir]
                                    (lspconfig.util.find_git_ancestor dir))
                        :settings {}}})

(local fennel-ls (get-server :fennel-ls))
(fennel-ls.setup {: capabilities})

;; More general LSP commands

;; fnlfmt: skip
(fn reload-lsp []
  (vim.lsp.stop_client (vim.lsp.get_active_clients))
  (vim.cmd.edit))

;; fnlfmt: skip
(fn open-lsp-log []
  (let [path (vim.lsp.get_log_path)]
    (vim.cmd.edit path)))

(command LspLog open-lsp-log)
(command LspRestart reload-lsp)
