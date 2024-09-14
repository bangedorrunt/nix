(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use neovim/nvim-lspconfig
       :event :BufReadPost
       :mod :lsp.config
       :dependencies
       [;; DEPRECATED in favour of Nix pkgs
        ;; :williamboman/mason.nvim
        :hrsh7th/cmp-nvim-lsp
        ])
  (use stevearc/conform.nvim :event :VeryLazy :mod :lsp.conform))

{: setup}
