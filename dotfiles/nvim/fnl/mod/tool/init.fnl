(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use ericpruitt/tmux.vim)
  (use aserowy/tmux.nvim (after-load mod.tool.tmux))
  (use gpanders/editorconfig.nvim))

{: setup}
