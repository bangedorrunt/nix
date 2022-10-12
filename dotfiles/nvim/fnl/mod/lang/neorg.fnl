(import-macros {: lazyreq : lazyfunc : augroup : autocmd : autocmd!}
               :core.macros)

(local neorg (lazyreq :neorg))
(local {: on_event} (lazyfunc :neorg.callbacks))
(fn setup []
  (neorg.setup
    {:load {:core.defaults {}
            :core.norg.concealer {:config {:dim_code_blocks {:conceal false}
                                           :icons {:heading {:level_1 {:icon ""}
                                                             :level_2 {:icon " "}
                                                             :level_3 {:icon "  "}
                                                             :level_4 {:icon "   "}
                                                             :level_5 {:icon "    "}
                                                             :level_6 {:icon "     "}}
                                                   :list {:level_1 {:icon ""}
                                                          :level_2 {:icon " "}
                                                          :level_3 {:icon "  "}
                                                          :level_4 {:icon "   "}
                                                          :level_5 {:icon "    "}
                                                          :level_6 {:icon "     "}}
                                                   :marker {:icon ""}
                                                   :todo {:done {:icon ""}
                                                          :pending {:icon ""}
                                                          :undone {:icon ""}
                                                          :uncertain {:icon ""}
                                                          :on_hold {:icon ""}
                                                          :cancelled {:icon ""}
                                                          :recurring {:icon ""}
                                                          :urgent {:icon ""}}}}}
            :core.norg.completion {:config {:engine :nvim-cmp}}
            :core.norg.dirman {:config {:workspaces {:notetoself :$HOME/workspace/notetoself
                                                     :gtd :$HOME/workspace/gtd}}}
            :core.gtd.base {:config {:workspace :gtd}}
            :core.integrations.telescope {}}})

  (on_event :core.keybinds.events.enable_keybinds
            (fn [_ keybinds]
              (keybinds.map_event_to_mode :norg
                                          {:n [[:<LocalLeader>zn
                                                :core.norg.dirman.new.note]
                                               [:<LocalLeader>zc
                                                :core.gtd.base.capture]
                                               [:<LocalLeader>ze
                                                :core.gtd.base.edit]
                                               [:<LocalLeader>zv
                                                :core.gtd.base.views]
                                               ["[d"
                                                  :core.integrations.treesitter.previous.heading]
                                                ["]d"
                                                :core.integrations.treesitter.next.heading]
                                               [:K
                                                :core.norg.esupports.hop.hop-link]
                                               [:gd
                                                :core.norg.esupports.hop.hop-link]
                                               [:<LocalLeader>zdz
                                                :core.norg.qol.todo_items.todo.task_cycle]
                                               [:<LocalLeader>zdd
                                                :core.norg.qol.todo_items.todo.task_done]
                                               [:<LocalLeader>zdu
                                                :core.norg.qol.todo_items.todo.task_undone]
                                               [:<LocalLeader>zdp
                                                :core.norg.qol.todo_items.todo.task_pending]
                                               [:<LocalLeader>zdh
                                                :core.norg.qol.todo_items.todo.task_on_hold]
                                               [:<LocalLeader>zdc
                                                :core.norg.qol.todo_items.todo.task_cancelled]
                                               [:<LocalLeader>zdr
                                                :core.norg.qol.todo_items.todo.task_recurring]
                                               [:<LocalLeader>zdi
                                                :core.norg.qol.todo_items.todo.task_important]
                                               [:<LocalLeader>f
                                                :core.integrations.telescope.find_linkable]]
                                           :i [[:<C-l>
                                                :core.integrations.telescope.insert_link]]}
                                          {:silent true
                                           :nowait true
                                           :noremap true}))))

{: setup}
