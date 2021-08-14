(module plugins.lsp.init
        {autoload {core aniseed.core
                   lspconfig lspconfig
                   signature lsp_signature}
         require-macros [core.macros]})

(include :plugins.lsp.ui)

(defn- enhanced-attach [client bufnr]
       (lua-command! LspHover "vim.lsp.buf.hover()")
       (lua-command! LspRename "vim.lsp.buf.rename()")
       (lua-command! LspTypeDef "vim.lsp.buf.type_definition()")
       (lua-command! LspImplementation "vim.lsp.buf.implementation()")
       (lua-command! LspDiagPrev "tdt.lsp.prev_diagnostic()")
       (lua-command! LspDiagNext "tdt.lsp.next_diagnostic()")
       (lua-command! LspDiagLine
                     "vim.lsp.diagnostic.show_line_diagnostics(tdt.lsp.popup_opts)")
       (lua-command! LspSignatureHelp "vim.lsp.buf.signature_help()")
       (buf-map! bufnr [n] :gy ":LspTypeDef<CR>")
       (buf-map! bufnr [n] :gi ":LspRename<CR>")
       (buf-map! bufnr [n] :K ":LspHover<CR>")
       (buf-map! bufnr [n] "[a" ":LspDiagPrev<CR>")
       (buf-map! bufnr [n] "]a" ":LspDiagNext<CR>")
       (buf-map! bufnr [i] :<C-x><C-x> :<Cmd>LspSignatureHelp<CR>)
       ;; FIXME have no idea this line will cancel out all commands
       ;; defined above. `CursorHold` at fault???
       ;; (buf-augroup! LspAutocommands (buf-autocmd! CursorHold "LspDiagLine"))
       (signature.on_attach {:bind true
                             :fix_pos true
                             :hint_enable false
                             :handler_opts {:border :single}})
       (if client.resolved_capabilities.document_formatting
           (do
             (buf-augroup! LspFormatOnSave
                           (buf-autocmd! BufWritePost
                                         "lua vim.lsp.buf.formatting()"))
             (buf-map! bufnr [n] :<Leader>lf
                       "<Cmd>lua vim.lsp.buf.formatting()<CR>"))
           client.resolved_capabilities.document_range_formatting
           (do
             (buf-augroup! LspFormatOnSave
                           (buf-autocmd! BufWritePost
                                         "lua vim.lsp.buf.range_formatting()"))
             (buf-map! bufnr [n] :<Leader>lf
                       "<Cmd>lua vim.lsp.buf.range_formatting()<CR>"))))

(def- capabilities (vim.lsp.protocol.make_client_capabilities))
;; Delegate snippet support to any completion engine such as `nvim-compe`
(set capabilities.textDocument.completion.completionItem.snippetSupport true)
(set capabilities.textDocument.completion.completionItem.resolveSupport
     {:properties [:documentation :detail :additionalTextEdits]})

;; Code action
(set capabilities.textDocument.codeAction
     {:dynamicRegistration true
      :codeActionLiteralSupport {:codeActionKind {:valueSet ((fn []
                                                               (def- res
                                                                     (vim.tbl_values vim.lsp.protocol.CodeActionKind))
                                                               (table.sort res)
                                                               res))}}})

(def- servers {:null-ls {:debounce 150
                        :sources ((fn []
                                    ;; NOTE: When you experience any lag or unresponsive with Lsp,
                                    ;; make sure respective sources are installed
                                    ;; In my case:
                                    ;; Typescript was slow because `eslint_d` was not installed
                                    ;; Markdown was slow because `write-good` and `markdownlint`
                                    ;; was not installed
                                    (def- null-ls (require :null-ls))
                                    (def- b null-ls.builtins)
                                    [(b.formatting.prettier.with {:filetypes [:html
                                                                              :json
                                                                              :yaml
                                                                              :markdown]})
                                     b.formatting.stylua
                                     b.formatting.prettier_d_slim
                                     b.formatting.fnlfmt
                                     (b.formatting.trim_whitespace.with {:filetypes [:tmux
                                                                                     :fish
                                                                                     :teal]})
                                     b.formatting.shfmt
                                     ;; b.diagnostics.write_good
                                     b.diagnostics.markdownlint
                                     (b.diagnostics.shellcheck.with {:filetypes [:zsh
                                                                                 :sh
                                                                                 :bash]})]))}
              :bashls {}
              :tsserver {:cmd {1 :typescript-language-server 2 :--stdio}
                         :on_attach (fn [client bufnr]
                                      (def- ts-utils
                                            (require :nvim-lsp-ts-utils))
                                      (def- ts-utils-settings
                                            {:enable_import_on_completion true
                                             :complete_parens true
                                             :signature_help_in_parens true
                                             :eslint_bin :eslint_d
                                             :eslint_enable_diagnostics true
                                             :enable_formatting true
                                             :formatter :eslint_d
                                             :update_imports_on_move true})
                                      (set client.resolved_capabilities.document_formatting
                                           false)
                                      (set client.resolved_capabilities.document_range_formatting
                                           false)
                                      (enhanced-attach client)
                                      (ts-utils.setup ts-utils-settings)
                                      (ts-utils.setup_client client)
                                      (buf-map! bufnr [n] :gs
                                                ":TSLspOrganize<CR>")
                                      (buf-map! bufnr [n] :gI
                                                ":TSLspRenameFile<CR>")
                                      (buf-map! bufnr [n] :go
                                                ":TSLspImportAll<CR>")
                                      (buf-map! bufnr [n] :qq
                                                ":TSLspFixCurrent<CR>")
                                      (buf-map! bufnr [n] "." :.<C-x><C-o>))}
              :cssls {:cmd [:css-languageserver :--stdio]}
              :rnix {}
              :jsonls {:cmd [:vscode-json-languageserver :--stdio]}
              :html {:cmd [:html-languageserver :--stdio]}
              :clangd {}
              ;; FIXME support `fennel`
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
                                                                                        :workspace {:library (vim.list_extend {(vim.fn.expand :$VIMRUNTIME/lua) true}
                                                                                                                              {})}}}}})
              :vimls {}
              :tailwindcss {:cmd [:tailwind-lsp]}})

(each [server config (pairs servers)]
  (let [lsp-server (. lspconfig server)]
    (if (not= server :null-ls)
        (lsp-server.setup (core.merge {:on_attach enhanced-attach
                                       : capabilities
                                       :flags {:debounce_text_changes 150}}
                                      config))
        (do
          ((. (require :null-ls) :config) (core.merge {} config))
          ;; (lsp-server.setup {:on_attach enhanced-attach})))))
          ((. (. lspconfig server) :setup) {:on_attach enhanced-attach})))))
