(import-macros {: lazyreq : after! : setup!} :core.macros)

(local plugins
  [[:neovim/nvim-lspconfig]
   [:williamboman/mason.nvim]
   [:williamboman/mason-lspconfig.nvim]
   [:jose-elias-alvarez/null-ls.nvim]])

(fn setup []
  (after! :mason-lspconfig.nvim (setup! :mod.lsp.lsp))
  (after! :null-ls.nvim (setup! :mod.lsp.null-ls)))

{: plugins
 : setup}
