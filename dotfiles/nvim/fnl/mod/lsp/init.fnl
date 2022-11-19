(import-macros {: use} :core.macros)

(fn setup []
  (use williamboman/mason.nvim :module :mason)
  (use jose-elias-alvarez/null-ls.nvim :module :null-ls)
  (use neovim/nvim-lspconfig :event :BufReadPost :init+ :lsp.config))

{: setup}
