(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use neovim/nvim-lspconfig)
  (use williamboman/mason.nvim)
  (use williamboman/mason-lspconfig.nvim (after-load mod.lsp.lsp))
  (use jose-elias-alvarez/null-ls.nvim (after-load mod.lsp.null-ls)))

{: setup}
