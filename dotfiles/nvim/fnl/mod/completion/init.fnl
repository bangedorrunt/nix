(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use L3MON4D3/LuaSnip
       :mod :completion.luasnip
       :dependencies :rafamadriz/friendly-snippets)
  (use hrsh7th/nvim-cmp
       :event :InsertEnter
       :mod :completion.nvim-cmp
       :dependencies
       [:hrsh7th/cmp-cmdline
        :hrsh7th/cmp-path
        :hrsh7th/cmp-buffer
        :hrsh7th/cmp-calc
        :hrsh7th/cmp-nvim-lsp
        :hrsh7th/cmp-nvim-lua
        :saadparwaiz1/cmp_luasnip
        :L3MON4D3/LuaSnip
        ]))

{: setup}
