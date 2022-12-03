(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use williamboman/mason.nvim :event "User LspLoaded")
  (use jose-elias-alvarez/null-ls.nvim :event "User LspLoaded")
  (use neovim/nvim-lspconfig :event :BufReadPost :init+ :lsp.config))
{: setup}
