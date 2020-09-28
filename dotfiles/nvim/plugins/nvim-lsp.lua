-- NVIM-LSP CONFIG
-- ===============

local diagnostic = require('diagnostic')
local completion = require('completion')
local nvim_lsp   = require('nvim_lsp')

local on_attach = function(client, bufnr)
  diagnostic.on_attach(client, bufnr)
  completion.on_attach(client, bufnr)
end

nvim_lsp.vimls.setup{
  on_attach = on_attach
}
nvim_lsp.bashls.setup{
  on_attach = on_attach
}
nvim_lsp.clojure_lsp.setup{
  on_attach = on_attach
}
nvim_lsp.sumneko_lua.setup{
  on_attach = on_attach
}
nvim_lsp.clangd.setup{
  on_attach = on_attach
}
nvim_lsp.pyls.setup{
  cmd = {"pyls", "--log-file", "/tmp/pyls-log.txt", "--verbose"},
  on_attach = on_attach
}

require'nvim-treesitter.configs'.setup {
  ensure_installed = "all",     -- One of "all", "language", or a list of languages
  highlight = {
    enable = true,              -- Set false will disable the whole extension
    disable = { "rust" },       -- List of language that will be disabled
  }
}

-- lsputils
vim.lsp.callbacks['textDocument/references'] = require'lsputil.locations'.references_handler
vim.lsp.callbacks['textDocument/definition'] = require'lsputil.locations'.definition_handler
vim.lsp.callbacks['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
vim.lsp.callbacks['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
vim.lsp.callbacks['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
vim.lsp.callbacks['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
vim.lsp.callbacks['workspace/symbol'] = require'lsputil.symbols'.workspace_handler
