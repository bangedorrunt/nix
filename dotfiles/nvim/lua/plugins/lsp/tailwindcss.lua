-- REF: https://github.com/mattn/vim-lsp-settings/blob/408414b382b7727d69151892b20718efa95a4a5a/installer/install-tailwindcss-intellisense.sh

local cmd = { ttd.paths.CACHE_DIR .. 'lsp_servers/tailwindcss-language-server/tailwindcss-intellisense', '--stdio' }

local M = {}
M.setup = function(on_attach, capabilities)
  require('lspconfig').tailwindcss.setup {
    cmd = cmd,
    on_attach = on_attach,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    },
  }
end

return M
