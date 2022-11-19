(import-macros {: use} :core.macros)

(fn setup []
  (use L3MON4D3/LuaSnip :module :luasnip)
  (use hrsh7th/nvim-cmp :module :cmp :event :InsertEnter :init+ :completion.nvim-cmp)
  (use hrsh7th/cmp-cmdline :event "User PackerDefered")
  (use hrsh7th/cmp-path :event "User PackerDefered")
  (use hrsh7th/cmp-buffer :event "User PackerDefered")
  (use hrsh7th/cmp-calc :event "User PackerDefered")
  (use hrsh7th/cmp-nvim-lsp :module :cmp_nvim_lsp)
  (use hrsh7th/cmp-nvim-lua :event "User PackerDefered")
  (use PaterJason/cmp-conjure :event "User PackerDefered")
  (use saadparwaiz1/cmp_luasnip :event "User PackerDefered"))

{: setup}
