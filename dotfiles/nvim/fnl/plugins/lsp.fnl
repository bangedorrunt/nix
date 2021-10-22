(module plugins.lsp.init
        {autoload {lsp/config lspconfig
                   lsp/signature lsp_signature
                   : null-ls
                   null-ls/builtins null-ls.builtins}
         require-macros [core.macros]})

;;;; LSP UI
(def popup-opts {:border :single :focusable false})

(tset vim.lsp.handlers :textDocument/signatureHelp
      (vim.lsp.with vim.lsp.handlers.signature_help popup-opts))

(tset vim.lsp.handlers :textDocument/hover
      (vim.lsp.with vim.lsp.handlers.hover popup-opts))

; More general LSP commands
(defn reload-lsp [] (vim.lsp.stop_client (vim.lsp.get_active_clients))
      (vim.cmd :edit))

(defn open-lsp-log [] (let [path (vim.lsp.get_log_path)]
                        (vim.cmd (string.format "edit %s" path))))

(command LspLog (viml->fn open-lsp-log))
(command LspRestart (viml->fn reload-lsp))

(noremap n nowait :<Leader>li :<Cmd>LspInfo<CR>)
(noremap n nowait :<Leader>ls :<Cmd>LspStart<CR>)
(noremap n nowait :<Leader>ll :<Cmd>LspLog<CR>)
(noremap n nowait :<Leader>lr :<Cmd>LspRestart<CR>)

;;;; LSP diagnostics
(vim.diagnostic.config {:underline true
                        :virtual_text {:spacing 4 :prefix "●"}
                        :float  {:show_header  true
                                 :source :if_many
                                 :border :single
                                 :focusable false}
                        :sign true
                        :severity_sort true})

(vim.fn.sign_define :DiagnosticSignError
                     {:text " " :texthl :DiagnosticSignError})

(vim.fn.sign_define :DiagnosticSignWarning
                     {:text " " :texthl :DiagnosticSignWarning})

(vim.fn.sign_define :DiagnosticSignInformation
                     {:text " " :texthl :DiagnosticSignInformation})

(vim.fn.sign_define :DiagnosticSignHint
                     {:text " " :texthl :DiagnosticSignHint})

;;;; LSP server config
(defn capable? [client capability] (. client.resolved_capabilities capability))

(def capabilities (let [c (vim.lsp.protocol.make_client_capabilities)]
                    ; NOTE: use `cmp_nvim_lsp.update_capabilities` is unneccessary
                    ; https://github.com/hrsh7th/cmp-nvim-lsp/blob/f6f471898bc4b45eacd36eef9887847b73130e0e/lua/cmp_nvim_lsp/init.lua#L23
                    ; Delegate snippet support to any completion engine such as `nvim-cmp`
                    (set c.textDocument.completion.completionItem.snippetSupport
                         true)
                    (set c.textDocument.completion.completionItem.resolveSupport
                         {:properties [:documentation
                                       :detail
                                       :additionalTextEdits]})
                    (set c.textDocument.completion.completionItem.preselectSupport
                         true)
                    (set c.textDocument.completion.completionItem.insertReplaceSupport
                         true)
                    (set c.textDocument.completion.completionItem.deprecatedSupport
                         true)
                    (set c.textDocument.completion.completionItem.commitCharactersSupport
                         true)
                    (set c.textDocument.completion.completionItem.tagSupport
                         {:valueSet [1]})
                    c))

(defn enhanced-attach [client bufnr]
      ; LSP keymaps
      (command LspHover -buffer "lua vim.lsp.buf.hover()")
      (command LspRename -buffer "lua vim.lsp.buf.rename()")
      (command LspTypeDef -buffer "lua vim.lsp.buf.type_definition()")
      (command LspImplementation -buffer "lua vim.lsp.buf.implementation()")
      (command LspDiagPrev -buffer "lua vim.diagnostic.goto_prev({ float = true })")
      (command LspDiagNext -buffer "lua vim.diagnostic.goto_next({ float = true })")
      (command LspDiagLine -buffer "lua vim.diagnostic.open_float(0, { scope = 'line' })")
      (command LspSignatureHelp -buffer "lua vim.lsp.buf.signature_help()")
      (noremap n buffer :gy :<Cmd>LspTypeDef<CR>)
      (noremap n buffer :gi :<Cmd>LspRename<CR>)
      (noremap n buffer :K :<Cmd>LspHover<CR>)
      (noremap n buffer "[a" :<Cmd>LspDiagPrev<CR>)
      (noremap n buffer "]a" :<Cmd>LspDiagNext<CR>)
      (noremap i buffer :<C-x><C-x> :<Cmd>LspSignatureHelp<CR>)
      ; LSP format
      (if (capable? client :document_formatting)
          (do
            (augroup LspFormatOnSave
                         (autocmd! * <buffer>)
                         (autocmd BufWritePost <buffer>
                                  "lua vim.lsp.buf.formatting_sync()"))
            (noremap n buffer "<Leader>lf"
                     "<Cmd>lua vim.lsp.buf.formatting()<CR>"))
          (print "LSP not support formatting."))
      )

(defn null-attach [client bufnr]
      ; LSP format
      (if (capable? client :document_formatting)
          (do
            (augroup LspFormatOnSave
                         (autocmd! * <buffer>)
                         (autocmd BufWritePost <buffer>
                                  "lua vim.lsp.buf.formatting()"))
            (noremap n buffer "<Leader>lf"
                     "<Cmd>lua vim.lsp.buf.formatting()<CR>"))
          (print "LSP not support formatting.")))

; fnlfmt: skip
(def servers {:bashls {}
              :tsserver {:cmd [:typescript-language-server :--stdio]
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
                                            ; :filter_out_diagnostics_by_code ["80001"]
                                            })
                                      (set client.resolved_capabilities.document_formatting
                                           false)
                                      (set client.resolved_capabilities.document_range_formatting
                                           false)
                                      (enhanced-attach client)
                                      (ts-utils.setup ts-utils-settings)
                                      (ts-utils.setup_client client)
                                      (noremap n buffer :gs
                                               ":TSLspOrganize<CR>")
                                      (noremap n buffer :gI
                                               ":TSLspRenameFile<CR>")
                                      (noremap n buffer :go
                                               ":TSLspImportAll<CR>")
                                      (noremap n buffer :qq
                                               ":TSLspFixCurrent<CR>")
                                      (noremap n buffer "." :.<C-x><C-o>))}
              :cssls {:cmd [:vscode-css-language-server :--stdio]}
              :rnix {}
              :jsonls {:cmd [:vscode-json-language-server :--stdio]}
              :html {:cmd [:vscode-html-language-server :--stdio]}
              :clangd {}
              :clojure_lsp {:filetypes [:clojure :edn]}
              :sumneko_lua ((. (require :lua-dev) :setup) {:library {:vimruntime true
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
                                                                                        :workspace {:library {(vim.fn.expand :$VIMRUNTIME/lua) true}}}}}})
              :vimls {}
              :tailwindcss {:cmd [:tailwind-lsp]}})

(defn merge [t1 t2]
  (vim.tbl_deep_extend "force" t1 t2))

(each [server config (pairs servers)]
  (let [lsp/server (. lsp/config server)]
    (-> {:on_attach enhanced-attach
         : capabilities
         :flags {:debounce_text_changes 150}}
        (merge config)
        (lsp/server.setup))))

; fnlfmt: skip
(-> {:debounce 150
     :sources ((fn []
                 ; WARNING: when you experience any lag or unresponsive with Lsp,
                 ; make sure respective sources are installed
                 ; In my case:
                 ; Typescript was slow because `eslint_d` was not installed
                 ; Markdown was slow because `write-good` and `markdownlint`
                 ; was not installed
                 [(null-ls/builtins.formatting.prettier.with {:filetypes [:html
                                                                          :json
                                                                          :yaml
                                                                          :markdown]})
                  null-ls/builtins.formatting.stylua
                  ; null-ls/builtins.formatting.prettier_d_slim
                  null-ls/builtins.formatting.fnlfmt
                  null-ls/builtins.formatting.trim_whitespace
                  null-ls/builtins.formatting.shfmt
                  ; null-ls/builtins.diagnostics.write_good
                  ; null-ls/builtins.diagnostics.markdownlint
                  (null-ls/builtins.diagnostics.shellcheck.with {:filetypes [:zsh
                                                                             :sh
                                                                             :bash]})]))}
    (null-ls.config))

; Because `null-ls` is a custom Lsp server, it is not known at
; compile time, hence gotta use `(. lsp/config server)` to look up
(-> {:on_attach null-attach}
    ((. (. lsp/config :null-ls) :setup)))
