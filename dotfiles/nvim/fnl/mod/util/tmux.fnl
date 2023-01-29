(import-macros {: setup!} :core.macros)

(fn setup []
  (setup! tmux {:copy_sync {:enable true}
                :navigation {:enable_default_keybindings true}}))
{: setup}
