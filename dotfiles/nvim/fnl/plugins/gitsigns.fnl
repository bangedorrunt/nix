(module plugins.gitsigns
  {autoload {gitsigns gitsigns}})

(gitsigns.setup {:signs {:add {:hl :GitGutterAdd :text "▋"}
                         :change {:hl :GitGutterChange
                                  :text "▋"}
                         :delete {:hl :GitGutterDelete
                                  :text "▋"}
                         :topdelete {:hl :GitGutterDeleteChange
                                     :text "▔"}
                         :changedelete {:hl :GitGutterChange
                                        :text "▎"}}
                 :numhl true
                 :linehl false
                 ;; I don't use this keymaps
                 :keymaps {}
                 :watch_index {:interval 1000}
                 :current_line_blame false
                 :sign_priority 6
                 :update_debounce 100
                 :status_formatter nil
                 ;; This is deprecated
                 ;; :use_decoration_api true
                 :use_internal_diff true})
