-- NVIM-LSP CONFIG
-- ===============

local completion = require 'completion'
local lspconfig = require 'lspconfig'

local on_attach = function(client, bufnr)
  completion.on_attach(client, bufnr)
end

lspconfig.vimls.setup {
  on_attach = on_attach,
}
lspconfig.bashls.setup {
  on_attach = on_attach,
}
lspconfig.clojure_lsp.setup {
  on_attach = on_attach,
}
-- lspconfig.sumneko_lua.setup{
--   on_attach = on_attach
-- }

-- lspconfig.clangd.setup{
--   on_attach = on_attach
-- }

lspconfig.pyls.setup {
  cmd = { 'pyls', '--log-file', '/tmp/pyls-log.txt', '--verbose' },
  on_attach = on_attach,
}

require('nvim-treesitter.configs').setup {
  ensure_installed = {
    'bash',
    'javascript',
    'typescript',
    'tsx',
    'yaml',
    'json',
    'html',
    'css',
    'lua',
    'python',
    'cpp',
    'c',
  }, -- One of "all", "language", or a list of languages
  highlight = {
    enable = true, -- Set false will disable the whole extension
    disable = { 'rust' }, -- List of language that will be disabled
  },
}
