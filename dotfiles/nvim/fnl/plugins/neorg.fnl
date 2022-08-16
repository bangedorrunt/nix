(module plugins.neorg
  {autoload {{: setup} neorg
             {: on_event} neorg.callbacks}})

(setup
  {:load {:core.defaults {}
          :core.norg.concealer {}
          :core.norg.esupports.metagen {:config {:type :auto}}
          :core.norg.completion {:config {:engine :nvim-cmp}}
          :core.norg.dirman {:config {:workspaces {:learnyousome "~/workspace/notetoself/learn-you-some"
                                                   :gtd "~/workspace/notetoself/gtd"}}}
          :core.gtd.base {:config {:workspace :gtd}}
          :core.integrations.telescope {}}})

(on_event
  :core.keybinds.events.enable_keybinds
  (fn [_ keybinds]
    (keybinds.map_event_to_mode
      :norg
      {:n [[:<Leader>zn :core.norg.dirman.new.note]
           [:<Leader>zc :core.gtd.base.capture]
           [:<Leader>ze :core.gtd.base.edit]
           [:<Leader>zv :core.gtd.base.views]
           ["[d" :core.integrations.treesitter.previous.heading]
           ["]d" :core.integrations.treesitter.next.heading]
           [:K :core.norg.esupports.hop.hop-link]
           [:gd :core.norg.esupports.hop.hop-link]
           [:<Leader>zdz :core.norg.qol.todo_items.todo.task_cycle]
           [:<Leader>zdd :core.norg.qol.todo_items.todo.task_done]
           [:<Leader>zdu :core.norg.qol.todo_items.todo.task_undone]
           [:<Leader>zdp :core.norg.qol.todo_items.todo.task_pending]
           [:<Leader>zdh :core.norg.qol.todo_items.todo.task_on_hold]
           [:<Leader>zdc :core.norg.qol.todo_items.todo.task_cancelled]
           [:<Leader>zdr :core.norg.qol.todo_items.todo.task_recurring]
           [:<Leader>zdi :core.norg.qol.todo_items.todo.task_important]
           [:<Up> :core.norg.manoeuvre.item_up]
           [:<Down> :core.norg.manoeuvre.item_down]
           [:<Leader>f :core.integrations.telescope.find_linkable]]
       :i [[:<C-l> :core.integrations.telescope.insert_link]]}
      {:silent true
       :noremap true})))
