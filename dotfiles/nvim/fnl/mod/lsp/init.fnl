(import-macros {: use : after-loaded : setup!} :core.macros)

(fn setup []
  (use neovim/nvim-lspconfig)
  (use williamboman/mason.nvim)
  (use williamboman/mason-lspconfig.nvim (after-loaded mod.lsp.lsp))
  (use jose-elias-alvarez/null-ls.nvim (after-loaded mod.lsp.null-ls)))

{: setup}
