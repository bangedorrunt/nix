(import-macros {: augroup : autocmd : autocmd! : setup!} :core.macros)
(fn setup []
  (setup! neorg
    {:load {:core.defaults {}
            :core.concealer {:config {:icons {:heading {:level_1 {:icon ""}
                                                             :level_2 {:icon " ◉"}
                                                             :level_3 {:icon "  ✺"}
                                                             :level_4 {:icon "   "}
                                                             :level_5 {:icon "    "}
                                                             :level_6 {:icon "     "}}
                                                   :list {:level_1 {:icon "•"}
                                                          :level_2 {:icon " ▸"}
                                                          :level_3 {:icon "  •"}
                                                          :level_4 {:icon "   ▸"}
                                                          :level_5 {:icon "    ▸"}
                                                          :level_6 {:icon "     ▸"}}
                                                   :todo {:done {:icon ""}
                                                          :pending {:icon "-"}
                                                          :undone {:icon "×"}
                                                          :uncertain {:icon "?"}
                                                          :on_hold {:icon ""}
                                                          :cancelled {:icon ""}
                                                          :recurring {:icon ""}
                                                          :urgent {:icon ""}}}}}
            :core.completion {:config {:engine :nvim-cmp}}
            :core.dirman {:config {:workspaces {:notetoself :$HOME/workspace/notetoself
                                                     :gtd :$HOME/workspace/gtd}}}
            ;; :core.gtd.base {:config {:workspace :gtd}}
            :core.integrations.telescope {}}}))

{: setup}
