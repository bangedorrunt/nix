;; SEE https://github.com/gh-liu/dotfiles/blob/0327b4767ccbe7dbf0a4172cf40eb9ad210bed9a/xdg_config/nvim/snippets/luasnip/all.lua
(fn setup []
  (let [ls (require :luasnip)
        types (require :luasnip.util.types)
        from-lua (require :luasnip.loaders.from_lua)
        from-vsc (require :luasnip.loaders.from_vscode)]

    (ls.config.setup {:history true
                      ;; :store_selection_keys "<Tab>"
                      :region_check_events "CursorHold,InsertLeave"
                      :delete_check_events "TextChanged,InsertEnter"
                      :enable_autosnippets false
                      :ext_opts { types.choiceNode {:passive {:virt_text [["<-"
                                                                           :Comment]]}
                                                    :active {:virt_text [["<-"
                                                                          :ErrorMsg]]}}
                                 types.insertNode {:passive {:virt_text [["●"
                                                                          :Comment]]}
                                                   :active {:virt_text [["●"
                                                                         :WarningMsg]]}}}})

    (from-lua.load {:paths [store.paths.luasnip]})
    (from-vsc.lazy_load)
    ))
{: setup}
