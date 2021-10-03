(module plugins.lsp.init 
  {autoload {core aniseed.core
             nvim aniseed.nvim
             lsp/config lspconfig
             lsp/signature lsp_signature
             null-ls null-ls
             null-ls/builtins null-ls.builtins}
   require-macros [core.macros]})

;;;; LSP UI
(defn reload-lsp []
  (vim.lsp.stop_client (vim.lsp.get_active_clients))
  (vim.cmd :edit))

(defn open-lsp-log []
  (let [path (vim.lsp.get_log_path)]
    (vim.cmd (.. "edit " path))))

(def popup-opts {:border :single :focusable false})

(tset tdt :lsp {:popup_opts popup-opts
                :reload_lsp reload-lsp
                :open_lsp_log open-lsp-log})

(tset vim.lsp.handlers :textDocument/signatureHelp
      (vim.lsp.with vim.lsp.handlers.signature_help popup-opts))

(tset vim.lsp.handlers :textDocument/hover (vim.lsp.with vim.lsp.handlers.hover popup-opts))

;;;; LSP diagnostics

(vim.diagnostic.config {:underline true
                        :update_in_insert false
                        :virtual_text {:spacing 4 :prefix "●"} 
                        :sign true
                        :severity_sort true})

(vim.fn.sign_define :DiagnosticSignError
                    {:text " "
                     :texthl :DiagnosticSignError})
(vim.fn.sign_define :DiagnosticSignWarning
                    {:text " "
                     :texthl :DiagnosticSignWarning})
(vim.fn.sign_define :DiagnosticSignInformation
                    {:text " "
                     :texthl :DiagnosticSignInformation})
(vim.fn.sign_define :DiagnosticSignHint
                    {:text " "
                     :texthl :DiagnosticSignHint})

;;;; LSP server config
(defn capable? [client capability]
       (. client.resolved_capabilities capability))

(def capabilities (let [c (vim.lsp.protocol.make_client_capabilities)]
                     ;; NOTE: use `cmp_nvim_lsp.update_capabilities` is unneccessary
                     ;; https://github.com/hrsh7th/cmp-nvim-lsp/blob/f6f471898bc4b45eacd36eef9887847b73130e0e/lua/cmp_nvim_lsp/init.lua#L23
                     ;; Delegate snippet support to any completion engine such as `nvim-cmp`
                     (set c.textDocument.completion.completionItem.snippetSupport true)
                     (set c.textDocument.completion.completionItem.resolveSupport
                          {:properties [:documentation
                                        :detail
                                        :additionalTextEdits]})
                     (set c.textDocument.completion.completionItem.preselectSupport true)
                     (set c.textDocument.completion.completionItem.insertReplaceSupport true)
                     (set c.textDocument.completion.completionItem.deprecatedSupport true)
                     (set c.textDocument.completion.completionItem.commitCharactersSupport true)
                     (set c.textDocument.completion.completionItem.tagSupport {:valueSet [1]})
                     c))

(defn enhanced-attach [client bufnr] 
       ;; LSP keymaps
       (viml->lua-buf-command :LspHover "vim.lsp.buf.hover()")
       (viml->lua-buf-command :LspRename "vim.lsp.buf.rename()")
       (viml->lua-buf-command :LspTypeDef "vim.lsp.buf.type_definition()")
       (viml->lua-buf-command :LspImplementation "vim.lsp.buf.implementation()")
       (viml->lua-buf-command :LspDiagPrev
                           "vim.lsp.diagnostic.goto_prev({ popup_opts = tdt.lsp.popup_opts})")
       (viml->lua-buf-command :LspDiagNext
                           "vim.lsp.diagnostic.goto_next({ popup_opts = tdt.lsp.popup_opts})")
       (viml->lua-buf-command :LspDiagLine
                         "vim.lsp.diagnostic.show_line_diagnostics(tdt.lsp.popup_opts)")
       (viml->lua-buf-command :LspSignatureHelp "vim.lsp.buf.signature_help()")
       (buf-noremap [n] :gy :<Cmd>LspTypeDef<CR>)
       (buf-noremap [n] :gi :<Cmd>LspRename<CR>)
       (buf-noremap [n] :K :<Cmd>LspHover<CR>)
       (buf-noremap [n] "[a" :<Cmd>LspDiagPrev<CR>)
       (buf-noremap [n] "]a" :<Cmd>LspDiagNext<CR>)
       (buf-noremap [i] :<C-x><C-x> :<Cmd>LspSignatureHelp<CR>)
       ;; LSP format
       (if (capable? client :document_formatting)
           (do
             (buf-augroup LspFormatOnSave
                           (buf-autocmd BufWritePost
                                         "lua vim.lsp.buf.formatting()"))
             (buf-noremap [n] :<Leader>lf #(vim.lsp.buf.formatting)))
           (capable? client :document_range_formatting)
           (do
             (buf-augroup LspFormatOnSave
                           (buf-autocmd BufWritePost
                                         "lua vim.lsp.buf.range_formatting()"))
             (buf-noremap [v] :<Leader>lf #(vim.lsp.buf.range_formatting)))
           (print "LSP not support formatting."))
       ;; LSP code action
       (buf-noremap [n] :<Leader>la :<Cmd>CodeActionMenu<CR>)
       ;; LSP signature
       (lsp/signature.on_attach
         {:bind true
          :doc_lines 10
          :hint_enabled true
          :hint_prefix " "
          :hint_scheme :String
          :handler_opts
          {:border :single}
          :decorator {"`" "`"}})
       )

;; fnlfmt: skip
(def servers {:bashls {}
              :tsserver
              {:cmd [:typescript-language-server :--stdio]
               :on_attach (fn [client bufnr]
                              (var ts-utils
                                    (require :nvim-lsp-ts-utils))
                              (var ts-utils-settings
                                    {:enable_import_on_completion true
                                     :complete_parens true
                                     :signature_help_in_parens true
                                     :eslint_bin :eslint_d
                                     :eslint_enable_diagnostics true
                                     :eslint_disable_if_no_config true
                                     :enable_formatting true
                                     :formatter :eslint_d
                                     :update_imports_on_move true
                                     ;; :filter_out_diagnostics_by_code ["80001"]
                                     })
                              (set client.resolved_capabilities.document_formatting
                                   false)
                              (set client.resolved_capabilities.document_range_formatting
                                   false)
                              (enhanced-attach client)
                              (ts-utils.setup ts-utils-settings)
                              (ts-utils.setup_client client)
                              (buf-noremap [n] :gs
                                            ":TSLspOrganize<CR>")
                              (buf-noremap [n] :gI
                                            ":TSLspRenameFile<CR>")
                              (buf-noremap [n] :go
                                            ":TSLspImportAll<CR>")
                              (buf-noremap [n] :qq
                                            ":TSLspFixCurrent<CR>")
                              (buf-noremap [n] "." :.<C-x><C-o>))}
              :cssls {:cmd [:vscode-css-language-server :--stdio]}
              :rnix {}
              :jsonls {:cmd [:vscode-json-language-server :--stdio]}
              :html {:cmd [:vscode-html-language-server :--stdio]}
              :clangd {}
              :clojure_lsp {:filetypes [:clojure :edn]}
              :sumneko_lua
              ((. (require :lua-dev) :setup)
                {:library {:vimruntime true
                 :types true
                 :plugins true}
                 :lspconfig {:cmd [:lua-language-server]
                             :settings {:Lua {:diagnostics {:enable true
                                                            :globals [:tdt
                                                                      :packer_plugins
                                                                      :vim
                                                                      :use
                                                                      :describe
                                                                      :it
                                                                      :assert
                                                                      :before_each
                                                                      :after_each]}
                                              :runtime {:version :LuaJIT}
                                              :workspace {:library (vim.list_extend {(vim.fn.expand :$VIMRUNTIME/lua) true} {})}}}}})
              :vimls {}
              :tailwindcss {:cmd [:tailwind-lsp]}})

(each [server config (pairs servers)]
  (let [lsp/server (. lsp/config server)]
    (-> {:on_attach enhanced-attach
         : capabilities
         :flags {:debounce_text_changes 150}
         }
        (core.merge config)
        (lsp/server.setup))))

(-> {:debounce 150
     :sources ((fn []
                 ;; WARNING: when you experience any lag or unresponsive with Lsp,
                 ;; make sure respective sources are installed
                 ;; In my case:
                 ;; Typescript was slow because `eslint_d` was not installed
                 ;; Markdown was slow because `write-good` and `markdownlint`
                 ;; was not installed
                 [(null-ls/builtins.formatting.prettier.with {:filetypes [:html
                                                                          :json
                                                                          :yaml
                                                                          :markdown]})
                  null-ls/builtins.formatting.stylua
                  ;; null-ls/builtins.formatting.prettier_d_slim
                  null-ls/builtins.formatting.fnlfmt
                  (null-ls/builtins.formatting.trim_whitespace.with {:filetypes [:tmux
                                                                                 :fish
                                                                                 :teal]})
                  null-ls/builtins.formatting.shfmt
                  ;; null-ls/builtins.diagnostics.write_good
                  ;; null-ls/builtins.diagnostics.markdownlint
                  (null-ls/builtins.diagnostics.shellcheck.with {:filetypes [:zsh
                                                                             :sh
                                                                             :bash]})]))}
    (null-ls.config))

;; ;; Because `null-ls` is a custom Lsp server, it is not known at
;; ;; compile time, hence gotta use `(. lsp/config server)` to look up
(-> {:on_attach enhanced-attach}
    ((. (. lsp/config :null-ls) :setup)))


;; ;; More general LSP commands
;; (command :LspLog "call v:lua.tdt.lsp.open_lsp_log()")
;; (command :LspRestart "call v:lua.tdt.lsp.reload_lsp()")

(command :LspRestart #(reload-lsp))
(command :LspLog #(open-lsp-log))

(noremap [n :nowait] :<Leader>li :<Cmd>LspInfo<CR>)
(noremap [n :nowait] :<Leader>ls :<Cmd>LspStart<CR>)
(noremap [n :nowait] :<Leader>ll :<Cmd>LspLog<CR>)
(noremap [n :nowait] :<Leader>lr :<Cmd>LspRestart<CR>)
