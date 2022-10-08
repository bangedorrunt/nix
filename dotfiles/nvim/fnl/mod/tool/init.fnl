(import-macros {: use : after-loaded : setup!} :core.macros)

(fn setup []
  (use ericpruitt/tmux.vim)
  (use aserowy/tmux.nvim (after-loaded mod.tool.tmux))
  (use gpanders/editorconfig.nvim))

{: setup}
