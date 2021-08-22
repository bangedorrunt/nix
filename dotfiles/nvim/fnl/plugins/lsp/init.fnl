(module plugins.lsp.init {autoload {lspconfig lspconfig
                                    signature lsp_signature
                                    null-ls null-ls
                                    null-ls/builtins null-ls.builtins
                                    coq coq
                                    cljfun bulb}})

(import-macros {: nmap!
                : noremap!
                : buf-noremap!
                : augroup!
                : buf-augroup!
                : autocmd!
                : buf-autocmd!
                : command!
                : buf-command!
                : lua-command!
                : lua-buf-command!} :core.macros)

(require :plugins.lsp.ui)

(defn- capable? [client capability] (. client.resolved_capabilities capability))

(defn- enhanced-attach [client bufnr] ;; Lsp keymaps
       (lua-buf-command! :LspHover "vim.lsp.buf.hover()")
       (lua-buf-command! :LspRename "vim.lsp.buf.rename()")
       (lua-buf-command! :LspTypeDef "vim.lsp.buf.type_definition()")
       (lua-buf-command! :LspImplementation "vim.lsp.buf.implementation()")
       (lua-buf-command! :LspDiagPrev "tdt.lsp.prev_diagnostic()")
       (lua-buf-command! :LspDiagNext "tdt.lsp.next_diagnostic()")
       (lua-buf-command! :LspDiagLine
                         "vim.lsp.diagnostic.show_line_diagnostics(tdt.lsp.popup_opts)")
       (lua-buf-command! :LspSignatureHelp "vim.lsp.buf.signature_help()")
       (buf-noremap! [n] :gy :<Cmd>LspTypeDef<CR>)
       (buf-noremap! [n] :gi :<Cmd>LspRename<CR>)
       (buf-noremap! [n] :K :<Cmd>LspHover<CR>)
       (buf-noremap! [n] "[a" :<Cmd>LspDiagPrev<CR>)
       (buf-noremap! [n] "]a" :<Cmd>LspDiagNext<CR>)
       (buf-noremap! [i] :<C-x><C-x> :<Cmd>LspSignatureHelp<CR>)
       (signature.on_attach {:bind true
                             :fix_pos true
                             :hint_enable false
                             :handler_opts {:border :single}})
       (if (capable? client :document_formatting)
           (do
             (buf-augroup! LspFormatOnSave
                           (buf-autocmd! BufWritePost
                                         "lua vim.lsp.buf.formatting()"))
             (buf-noremap! [n] :<Leader>lf
                           "<Cmd>lua vim.lsp.buf.formatting()<CR>"))
           (capable? client :document_range_formatting)
           (do
             (buf-augroup! LspFormatOnSave
                           (buf-autocmd! BufWritePost
                                         "lua vim.lsp.buf.range_formatting()"))
             (buf-noremap! [n] :<Leader>lf
                           "<Cmd>lua vim.lsp.buf.range_formatting()<CR>"))))

(def- servers {:null-ls {:debounce 150
                         :sources ((fn []
                                     ;; NOTE: When you experience any lag or unresponsive with Lsp,
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
                                      null-ls/builtins.formatting.prettier_d_slim
                                      null-ls/builtins.formatting.fnlfmt
                                      (null-ls/builtins.formatting.trim_whitespace.with {:filetypes [:tmux
                                                                                                     :fish
                                                                                                     :teal]})
                                      null-ls/builtins.formatting.shfmt
                                      ;; null-ls/builtins.diagnostics.write_good
                                      null-ls/builtins.diagnostics.markdownlint
                                      (null-ls/builtins.diagnostics.shellcheck.with {:filetypes [:zsh
                                                                                                 :sh
                                                                                                 :bash]})]))}
               :bashls {}
               :tsserver {:cmd [:typescript-language-server :--stdio]
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
                                       (buf-noremap! [n] :gs
                                                     ":TSLspOrganize<CR>")
                                       (buf-noremap! [n] :gI
                                                     ":TSLspRenameFile<CR>")
                                       (buf-noremap! [n] :go
                                                     ":TSLspImportAll<CR>")
                                       (buf-noremap! [n] :qq
                                                     ":TSLspFixCurrent<CR>")
                                       (buf-noremap! [n] "." :.<C-x><C-o>))}
               :cssls {:cmd [:css-languageserver :--stdio]}
               :rnix {}
               :jsonls {:cmd [:vscode-json-languageserver :--stdio]}
               :html {:cmd [:html-languageserver :--stdio]}
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
                                                                                         :workspace {:library (vim.list_extend {(vim.fn.expand :$VIMRUNTIME/lua) true}
                                                                                                                               {})}}}}})
               :vimls {}
               :tailwindcss {:cmd [:tailwind-lsp]}})

(each [server config (pairs servers)]
  (let [lsp/server (. lspconfig server)]
    (if (not= server :null-ls)
        (-> {:on_attach enhanced-attach :flags {:debounce_text_changes 150}}
            (cljfun.merge config)
            (coq.lsp_ensure_capabilities)
            (lsp/server.setup))
        (do
          (-> {}
              (cljfun.merge config)
              (null-ls.config))
          ;; Because `null-ls` is a custom Lsp server, it is not known at
          ;; compile time, hence gotta use `(. lspconfig server)` to look up
          (-> {:on_attach enhanced-attach}
              ;; ((. (. lspconfig server) :setup)))))))
              ((-> server
                   ((partial . lspconfig))
                   (. :setup))))))))
