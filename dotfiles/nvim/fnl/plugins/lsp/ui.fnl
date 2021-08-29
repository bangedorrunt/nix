(module plugins.lsp.ui {autoload {lsp-diagnostics plugins.lsp.diagnostics
                                  lsp-kinds plugins.lsp.kinds}
                        require-macros [core.macros]})

(def- lsp vim.lsp)
(def- api vim.api)

(if lsp.setup
    (lsp.setup {:floating_preview {:border ["┌"
                                            "─"
                                            "┐"
                                            "│"
                                            "┘"
                                            "─"
                                            "└"
                                            "│"]}
                :diagnostics {:signs {:error " "
                                      :warning " "
                                      :hint " "
                                      :information " "}
                              :display {:underline true
                                        :update_in_insert false
                                        :virtual_text {:spacing 4
                                                       :prefix "●"}
                                        :severity_sort true}}
                :completion {:kind {:Class " "
                                    :Color " "
                                    :Constant " "
                                    :Constructor " "
                                    :Enum "了 "
                                    :EnumMember " "
                                    :Field " "
                                    :File " "
                                    :Folder " "
                                    :Function " "
                                    :Interface " "
                                    :Keyword " "
                                    :Method " "
                                    :Module " "
                                    :Property " "
                                    :Snippet "﬌ "
                                    :Struct " "
                                    :Text " "
                                    :Unit " "
                                    :Value " "
                                    :Variable " "}}})
    (do
      (lsp-diagnostics.setup)
      (lsp-kinds.setup)))

(set lsp.util.close_preview_autocmd
     (fn [events winnr]
       (set-forcibly! events
                      (vim.tbl_filter (fn [v]
                                        (and (not= v :CursorMovedI)
                                             (not= v :BufLeave)))
                                      events))
       (vim.api.nvim_command (.. "autocmd " (table.concat events ",")
                                 " <buffer> ++once lua pcall(vim.api.nvim_win_close, "
                                 winnr ", true)"))))

(def- popup-opts {:border :single :focusable false})

(tset lsp.handlers :textDocument/signatureHelp
      (lsp.with lsp.handlers.signature_help popup-opts))

(tset lsp.handlers :textDocument/hover (lsp.with lsp.handlers.hover popup-opts))

(tset tdt :lsp {: popup-opts})

;; More general LSP commands
(set _G.reload_lsp (fn []
                     (lsp.stop_client (vim.lsp.get_active_clients))
                     (vim.cmd :edit)))

(set _G.open_lsp_log (fn []
                       (let [path (vim.lsp.get_log_path)]
                         (vim.cmd (.. "edit " path)))))

(command! LspLog "call v:lua.open_lsp_log()")
(command! LspRestart "call v:lua.reload_lsp()")

(noremap! [n :nowait] :<Leader>li :<Cmd>LspInfo<CR>)
(noremap! [n :nowait] :<Leader>ls :<Cmd>LspStart<CR>)
(noremap! [n :nowait] :<Leader>ll :<Cmd>LspLog<CR>)
(noremap! [n :nowait] :<Leader>lr :<Cmd>LspRestart<CR>)
