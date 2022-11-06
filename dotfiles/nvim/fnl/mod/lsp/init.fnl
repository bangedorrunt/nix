(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use williamboman/mason.nvim)
  (use neovim/nvim-lspconfig (after-load mod.lsp.lsp))
  (use jose-elias-alvarez/null-ls.nvim (after-load mod.lsp.null-ls)))

{: setup}
