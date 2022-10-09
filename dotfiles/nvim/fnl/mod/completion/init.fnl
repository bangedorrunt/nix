(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use windwp/nvim-autopairs :module :nvim-autopairs.completion.cmp (after-load nvim-autopairs))
  (use L3MON4D3/LuaSnip)
  (use hrsh7th/nvim-cmp (after-load mod.completion.nvim-cmp))
  (use hrsh7th/cmp-cmdline)
  (use hrsh7th/cmp-path)
  (use hrsh7th/cmp-buffer)
  (use hrsh7th/cmp-calc)
  (use hrsh7th/cmp-nvim-lsp)
  (use hrsh7th/cmp-nvim-lua)
  (use PaterJason/cmp-conjure)
  (use saadparwaiz1/cmp_luasnip))

{: setup}
