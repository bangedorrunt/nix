(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use hrsh7th/nvim-cmp
       :event :InsertEnter
       :mod :completion.nvim-cmp
       :dependencies
       [
        :hrsh7th/cmp-cmdline
        :hrsh7th/cmp-path
        :hrsh7th/cmp-buffer
        :hrsh7th/cmp-calc
        :hrsh7th/cmp-nvim-lsp
        :hrsh7th/cmp-nvim-lua
        :L3MON4D3/LuaSnip
        :saadparwaiz1/cmp_luasnip
        ]))

{: setup}
