(import-macros {: lazyreq : after! : setup!} :core.macros)

(local plugins
  [[:L3MON4D3/LuaSnip]
   [:hrsh7th/nvim-cmp]
   [:hrsh7th/cmp-cmdline]
   [:hrsh7th/cmp-path]
   [:hrsh7th/cmp-buffer]
   [:hrsh7th/cmp-calc]
   [:hrsh7th/cmp-nvim-lsp]
   [:hrsh7th/cmp-nvim-lua]
   [:PaterJason/cmp-conjure]
   [:saadparwaiz1/cmp_luasnip]])

(fn setup []
  (after! :nvim-cmp (setup! :mod.completion.nvim-cmp)))

{: plugins
 : setup}
