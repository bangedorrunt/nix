(import-macros {: lazyreq} :core.macros)

(let [{: setup} (lazyreq :tmux)]
  (setup {:copy_sync {:enable true}
          :navigation {:enable_default_keybindings true}}))
