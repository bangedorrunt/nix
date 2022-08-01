(module plugins.gitsigns
  {autoload {{: setup} gitsigns}})

;; fnlfmt: skip
(setup {:signs {:add          {:text "+"}
                :change       {:text "~"}
                :delete       {:text "_"}
                :topdelete    {:text "‾"}
                :changedelete {:text "~"}}
        :preview_config {:border :solid
                         :style :minimal
                         :relative :cursor}
        :numhl true
        :linehl false
        ;; I don't use this keymaps
        :keymaps {}
        :watch_gitdir {:interval 1000}
        :current_line_blame false
        :sign_priority 6
        :update_debounce 100
        :status_formatter nil})

