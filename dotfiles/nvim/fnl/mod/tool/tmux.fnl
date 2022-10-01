(import-macros {: lazyreq} :core.macros)

(local tmux (lazyreq :tmux))

(fn setup []
  (tmux.setup {:copy_sync {:enable true}
               :navigation {:enable_default_keybindings true}}))
{: setup}
