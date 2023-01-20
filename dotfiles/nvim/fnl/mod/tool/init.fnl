(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use ericpruitt/tmux.vim :event "VeryLazy")
  (use aserowy/tmux.nvim :event "VeryLazy" :mod+ :tmux)
  ;; featured in 0.9 nightly
  ;; (use gpanders/editorconfig.nvim)
  )

{: setup}
