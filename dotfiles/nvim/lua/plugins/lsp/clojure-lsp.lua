local M = {}

M.setup = function(on_attach, capabilities)
  require('lspconfig').clojure_lsp.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    },
    filetypes = { 'clojure', 'edn' },
  }
end

return M
