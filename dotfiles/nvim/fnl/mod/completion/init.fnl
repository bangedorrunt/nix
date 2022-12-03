(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use L3MON4D3/LuaSnip :event "User CmpLoaded")
  (use saadparwaiz1/cmp_luasnip :event "User CmpLoaded"))
  (use hrsh7th/cmp-cmdline :event "User CmpLoaded")
  (use hrsh7th/cmp-path :event "User CmpLoaded")
  (use hrsh7th/cmp-buffer :event "User CmpLoaded")
  (use hrsh7th/cmp-calc :event "User CmpLoaded")
  (use hrsh7th/cmp-nvim-lsp :event "User LspLoaded")
  (use hrsh7th/cmp-nvim-lua :event "User CmpLoaded")
  (use PaterJason/cmp-conjure :event "User CmpLoaded")
  (use hrsh7th/nvim-cmp
       :event [:InsertEnter "User NeorgLoaded"]
       :init+ :completion.nvim-cmp)

{: setup}
