(import-macros {: lazyreq : after! : setup!} :core.macros)

(local plugins
  [[:ericpruitt/tmux.vim]
   [:aserowy/tmux.nvim :mod :tmux]
   [:gpanders/editorconfig.nvim]])

(fn setup []
  (after! :tmux.nvim (setup! :mod.tool.tmux)))

{: plugins
 : setup}
