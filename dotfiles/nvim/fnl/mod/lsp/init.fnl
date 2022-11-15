(import-macros {: use} :core.macros)

(fn setup []
  (use williamboman/mason.nvim)
  (use neovim/nvim-lspconfig :mod :lsp.config)
  (use jose-elias-alvarez/null-ls.nvim :mod :lsp.null-ls))

{: setup}
