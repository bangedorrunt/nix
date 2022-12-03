(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use ericpruitt/tmux.vim :event :BufReadPost)
  (use aserowy/tmux.nvim :event "User PackerDefered" :init :tmux)
  (use gpanders/editorconfig.nvim :start true))

{: setup}
