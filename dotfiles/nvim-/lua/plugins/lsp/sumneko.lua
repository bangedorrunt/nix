-- local global = require 'core.global'
local u = require 'core.utils'
local lspconfig = require 'lspconfig'

local luadev = function(on_attach, capabilities)
  return require('lua-dev').setup {
    -- Add any options here, or leave empty to use the default settings
    library = {
      vimruntime = true, -- Runtime path
      types = true, -- Full signature, docs and completion of vim.api, vim.treesitter, vim.lsp and others
      plugins = true, -- Installed opt or start plugins in packpath
      -- plugins = { 'nvim-treesitter', 'plenary.nvim', 'telescope.nvim' },
    },
    lspconfig = {
      on_attach = function(client, bufnr)
        on_attach(client)
        u.buf_map('i', '.', '.<C-x><C-o>', nil, bufnr)
      end,
      capabilities = capabilities,
      flags = {
        debounce_text_changes = 150,
      },
      cmd = {
        ttd.paths.CACHE_DIR .. 'lsp_servers/lua-language-server/bin/macOS/lua-language-server',
        '-E',
        ttd.paths.CACHE_DIR .. 'lsp_servers/lua-language-server/main.lua',
      },
      settings = {
        Lua = {
          diagnostics = {
            enable = true,
            globals = {
              'ttd',
              'packer_plugins',
              'vim',
              'use',
              'describe',
              'it',
              'assert',
              'before_each',
              'after_each',
            },
          },
          runtime = { version = 'LuaJIT' },
          workspace = {
            library = vim.list_extend({ [vim.fn.expand '$VIMRUNTIME/lua'] = true }, {}),
          },
        },
      },
    },
  }
end

local M = {}

M.setup = function(on_attach, capabilities)
  lspconfig.sumneko_lua.setup(luadev(on_attach, capabilities))
end

return M
