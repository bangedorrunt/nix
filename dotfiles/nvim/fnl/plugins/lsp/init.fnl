(module plugins.lsp.init {autoload {cljlib cljlib
                                    lspconfig lspconfig
                                    cmp/lsp cmp_nvim_lsp
                                    null-ls null-ls
                                    null-ls/builtins null-ls.builtins}
                          require-macros [core.macros]})

(def {: merge} cljlib)

(require :plugins.lsp.ui)

(defn- capable? [client capability] (. client.resolved_capabilities capability))

(def- capabilities
  (let [c (vim.lsp.protocol.make_client_capabilities)]
    ;; Delegate snippet support to any completion engine such as `nvim-cmp`
    (set c.textDocument.completion.completionItem.snippetSupport true)
    (set c.textDocument.completion.completionItem.resolveSupport {:properties [:documentation
                                                                               :detail
                                                                               :additionalTextEdits]})
    (set c.textDocument.completion.completionItem.preselectSupport true)
    (set c.textDocument.completion.completionItem.insertReplaceSupport true)
    (set c.textDocument.completion.completionItem.deprecatedSupport true)
    (set c.textDocument.completion.completionItem.commitCharactersSupport true)
    (set c.textDocument.completion.completionItem.tagSupport {:valueSet [1]})
    ;; Code action
    ;; (set c.textDocument.codeAction
    ;;      {:dynamicRegistration true
    ;;       :codeActionLiteralSupport {:codeActionKind {:valueSet ((fn []
    ;;                                                                (def- res
    ;;                                                                  (vim.tbl_values vim.lsp.protocol.CodeActionKind))
    ;;                                                                (table.sort res)
    ;;                                                                res))}}})
    (cmp/lsp.update_capabilities c)))

(defn- enhanced-attach [client bufnr]
       ;; Lsp keymaps
       (lua-buf-command! :LspHover "vim.lsp.buf.hover()")
       (lua-buf-command! :LspRename "vim.lsp.buf.rename()")
       (lua-buf-command! :LspTypeDef "vim.lsp.buf.type_definition()")
       (lua-buf-command! :LspImplementation "vim.lsp.buf.implementation()")
       (lua-buf-command! :LspDiagPrev "vim.lsp.diagnostic.goto_prev({ popup_opts = tdt.lsp.popup_opts})")
       (lua-buf-command! :LspDiagNext "vim.lsp.diagnostic.goto_next({ popup_opts = tdt.lsp.popup_opts})")
       (lua-buf-command! :LspDiagLine
                         "vim.lsp.diagnostic.show_line_diagnostics(tdt.lsp.popup_opts)")
       (lua-buf-command! :LspSignatureHelp "vim.lsp.buf.signature_help()")
       (buf-noremap! [n] :gy :<Cmd>LspTypeDef<CR>)
       (buf-noremap! [n] :gi :<Cmd>LspRename<CR>)
       (buf-noremap! [n] :K :<Cmd>LspHover<CR>)
       (buf-noremap! [n] "[a" :<Cmd>LspDiagPrev<CR>)
       (buf-noremap! [n] "]a" :<Cmd>LspDiagNext<CR>)
       (buf-noremap! [i] :<C-x><C-x> :<Cmd>LspSignatureHelp<CR>)
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
                           "<Cmd>lua vim.lsp.buf.range_formatting()<CR>"))
           (print "LSP not support formatting.")))
;; fnlfmt: skip
(def- servers {:bashls
               {}

               :tsserver
               {:cmd [:typescript-language-server :--stdio]
                :on_attach (fn [client bufnr]
                               (def- ts-utils
                                     (require :nvim-lsp-ts-utils))
                               (def- ts-utils-settings
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
                               (buf-noremap! [n] :gs
                                             ":TSLspOrganize<CR>")
                               (buf-noremap! [n] :gI
                                             ":TSLspRenameFile<CR>")
                               (buf-noremap! [n] :go
                                             ":TSLspImportAll<CR>")
                               (buf-noremap! [n] :qq
                                             ":TSLspFixCurrent<CR>")
                               (buf-noremap! [n] "." :.<C-x><C-o>))}

               :cssls
               {:cmd [:css-languageserver :--stdio]}

               :rnix
               {}

               :jsonls
               {:cmd [:vscode-json-languageserver :--stdio]}

               :html
               {:cmd [:html-languageserver :--stdio]}

               :clangd
               {}

               :clojure_lsp
               {:filetypes [:clojure :edn]}

               ;; :sumneko_lua
               ;; {
               ;;  :cmd [(.. tdt.paths.CACHE_DIR "/lsp_servers/lua-language-server/bin/macOS/lua-language-server")
               ;;        "-E"
               ;;        (.. tdt.paths.CACHE_DIR "/lsp_servers/lua-language-server/main.lua")]
               ;; }
               :sumneko_lua
               ((. (require :lua-dev) :setup)
                 {:library {:vimruntime true
                  :types true
                  :plugins true}
                  :lspconfig {;; Temporarily use custom install until sumneko nix overlay is fixed
                              :cmd [(.. tdt.paths.CACHE_DIR "/lsp_servers/lua-language-server/bin/macOS/lua-language-server")
                                   "-E"
                                   (.. tdt.paths.CACHE_DIR "/lsp_servers/lua-language-server/main.lua")]
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
               :vimls
               {}

               :tailwindcss
               {:cmd [:tailwind-lsp]}})

(each [server config (pairs servers)]
  (let [lsp/server (. lspconfig server)]
        (-> {:on_attach enhanced-attach
             : capabilities
             :flags {:debounce_text_changes 150}}
            (merge config)
            ;; Experimenting nvim-cmp
            ;; (coq.lsp_ensure_capabilities)
            (lsp/server.setup))))

(-> {:debounce 150
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
    (null-ls.config))

;; ;; Because `null-ls` is a custom Lsp server, it is not known at
;; ;; compile time, hence gotta use `(. lspconfig server)` to look up
(-> {:on_attach enhanced-attach}
    ((. (. lspconfig :null-ls) :setup)))
