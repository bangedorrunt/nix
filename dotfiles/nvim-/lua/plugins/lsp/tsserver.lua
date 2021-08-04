local ts_utils = require 'nvim-lsp-ts-utils'

local u = require 'core.utils'

local cmd = { 'typescript-language-server', '--stdio' }

local ts_utils_settings = {
  -- debug = true,
  enable_import_on_completion = true,
  complete_parens = true,
  signature_help_in_parens = true,
  eslint_bin = 'eslint_d',
  eslint_enable_diagnostics = true,
  enable_formatting = true,
  formatter = 'eslint_d',
  update_imports_on_move = true,
}

local M = {}
M.setup = function(on_attach, capabilities)
  require('lspconfig').tsserver.setup {
    cmd = cmd,
    on_attach = function(client, bufnr)
      client.resolved_capabilities.document_formatting = false
      on_attach(client)
      -- If `Typescript` is slow, make sure `eslint_d` installed
      ts_utils.setup(ts_utils_settings)
      ts_utils.setup_client(client)

      u.buf_map('n', 'gs', ':TSLspOrganize<CR>', nil, bufnr)
      u.buf_map('n', 'gI', ':TSLspRenameFile<CR>', nil, bufnr)
      u.buf_map('n', 'go', ':TSLspImportAll<CR>', nil, bufnr)
      u.buf_map('n', 'qq', ':TSLspFixCurrent<CR>', nil, bufnr)
      u.buf_map('i', '.', '.<C-x><C-o>', nil, bufnr)

      -- vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
    end,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    },
  }
end

return M
