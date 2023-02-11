(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use neovim/nvim-lspconfig
       :event :BufReadPost
       :mod :lsp.config
       :dependencies
       [;; :williamboman/mason.nvim
        :jose-elias-alvarez/null-ls.nvim
        :hrsh7th/cmp-nvim-lsp
        ]))
{: setup}
