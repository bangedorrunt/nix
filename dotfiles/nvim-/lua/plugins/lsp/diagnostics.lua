-- Automatically update diagnostics
vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  -- Enable underline, use default values
  underline = true,
  -- Disable a feature
  update_in_insert = false,
  -- Enable virtual text, override spacing to 4
  virtual_text = { spacing = 4, prefix = '●' },
  severity_sort = true,
})

local signs = {
  Error = ' ',
  Warning = ' ',
  Hint = ' ',
  Information = ' ',
}

for type, icon in pairs(signs) do
  local hl = 'LspDiagnosticsSign' .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = '' })
end
