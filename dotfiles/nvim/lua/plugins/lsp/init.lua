-- REF: https://github.com/akinsho/dotfiles/blob/01ddf20de97e29cbada48a819f0f6d980c6b7abe/.config/nvim/lua/as/plugins/lspconfig.lua
local lspconfig = require 'lspconfig'
local u = require 'core.utils'

local cmd, api, lsp = vim.cmd, vim.api, vim.lsp

if lsp.setup then
  lsp.setup {
    floating_preview = {
      border = { '┌', '─', '┐', '│', '┘', '─', '└', '│' },
    },
    diagnostics = {
      signs = {
        error = ' ',
        warning = ' ',
        hint = ' ',
        information = ' ',
      },
      display = {
        underline = true,
        update_in_insert = false,
        virtual_text = { spacing = 4, prefix = '●' },
        severity_sort = true,
      },
    },
    completion = {
      kind = {
        Class = ' ',
        Color = ' ',
        Constant = ' ',
        Constructor = ' ',
        Enum = '了 ',
        EnumMember = ' ',
        Field = ' ',
        File = ' ',
        Folder = ' ',
        Function = ' ',
        Interface = 'ﰮ ',
        Keyword = ' ',
        Method = 'ƒ ',
        Module = ' ',
        Property = ' ',
        Snippet = '﬌ ',
        Struct = ' ',
        Text = ' ',
        Unit = ' ',
        Value = ' ',
        Variable = ' ',
      },
    },
  }
else
  require 'plugins.lsp.diagnostics'
  require('plugins.lsp.kind').setup()
end

lsp.util.close_preview_autocmd = function(events, winnr)
  events = vim.tbl_filter(function(v)
    return v ~= 'CursorMovedI' and v ~= 'BufLeave'
  end, events)

  api.nvim_command(
    'autocmd '
      .. table.concat(events, ',')
      .. ' <buffer> ++once lua pcall(vim.api.nvim_win_close, '
      .. winnr
      .. ', true)'
  )
end

local popup_opts = { border = 'single', focusable = false }

local peek_definition = function()
  vim.lsp.buf_request(0, 'textDocument/definition', lsp.util.make_position_params(), function(_, _, result)
    if result == nil or vim.tbl_isempty(result) then
      return nil
    end
    lsp.util.preview_location(result[1], popup_opts)
  end)
end

lsp.handlers['textDocument/signatureHelp'] = lsp.with(lsp.handlers.signature_help, popup_opts)
lsp.handlers['textDocument/hover'] = lsp.with(lsp.handlers.hover, popup_opts)

local go_to_diagnostic = function(pos)
  return pos and api.nvim_win_set_cursor(0, { pos[1] + 1, pos[2] })
end

local next_diagnostic = function()
  go_to_diagnostic(lsp.diagnostic.get_next_pos() or lsp.diagnostic.get_prev_pos())
end

local prev_diagnostic = function()
  go_to_diagnostic(lsp.diagnostic.get_prev_pos() or lsp.diagnostic.get_next_pos())
end

ttd.lsp = {
  popup_opts = popup_opts,
  peek_definition = peek_definition,
  next_diagnostic = next_diagnostic,
  prev_diagnostic = prev_diagnostic,
}

local function on_attach(client, bufnr)
  -- Commands
  u.lua_command('LspPeekDef', 'ttd.lsp.peek_definition()')
  u.lua_command('LspFormatting', 'vim.lsp.buf.formatting()')
  u.lua_command('LspHover', 'vim.lsp.buf.hover()')
  u.lua_command('LspRename', 'vim.lsp.buf.rename()')
  u.lua_command('LspTypeDef', 'vim.lsp.buf.type_definition()')
  u.lua_command('LspImplementation', 'vim.lsp.buf.implementation()')
  u.lua_command('LspDiagPrev', 'ttd.lsp.prev_diagnostic()')
  u.lua_command('LspDiagNext', 'ttd.lsp.next_diagnostic()')
  u.lua_command('LspDiagLine', 'vim.lsp.diagnostic.show_line_diagnostics(ttd.lsp.popup_opts)')
  u.lua_command('LspSignatureHelp', 'vim.lsp.buf.signature_help()')

  -- Bindings
  u.buf_map('n', 'gh', ':LspPeekDef<CR>', nil, bufnr)
  u.buf_map('n', 'gy', ':LspTypeDef<CR>', nil, bufnr)
  u.buf_map('n', 'gi', ':LspRename<CR>', nil, bufnr)
  u.buf_map('n', 'K', ':LspHover<CR>', nil, bufnr)
  u.buf_map('n', '[a', ':LspDiagPrev<CR>', nil, bufnr)
  u.buf_map('n', ']a', ':LspDiagNext<CR>', nil, bufnr)
  u.buf_map('i', '<C-x><C-x>', '<Cmd> LspSignatureHelp<CR>', nil, bufnr)

  u.buf_augroup('LspAutocommands', 'CursorHold', 'LspDiagLine')

  if client.resolved_capabilities.document_formatting then
    u.buf_augroup('LspFormatOnSave', 'BufWritePost', 'lua vim.lsp.buf.formatting()')
    u.map('n', '<Leader>lf', '<Cmd>lua vim.lsp.buf.formatting()<CR>')
  elseif client.resolved_capabilities.document_range_formatting then
    u.buf_augroup('LspFormatOnSave', 'BufWritePost', 'lua vim.lsp.buf.range_formatting()')
    u.map('n', '<Leader>lf', '<Cmd>lua vim.lsp.buf.range_formatting()<CR>')
  end
  require('plugins.lsp.completion').setup(client, bufnr)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
-- Broadcasting snippet support
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = { 'documentation', 'detail', 'additionalTextEdits' },
}
-- Code actions
capabilities.textDocument.codeAction = {
  dynamicRegistration = true,
  codeActionLiteralSupport = {
    codeActionKind = {
      valueSet = (function()
        local res = vim.tbl_values(vim.lsp.protocol.CodeActionKind)
        table.sort(res)
        return res
      end)(),
    },
  },
}

local servers = {
  -- Cannot use `null-ls` because it's not Lsp config
  ['null-ls'] = {
    debounce = 150,
    sources = (function()
      local null_ls = require 'null-ls'
      local b = null_ls.builtins
      -- NOTE: When you experience any lag or unresponsive with Lsp,
      ---- make sure respective sources are installed
      ---- In my case:
      ---- Typescript was slow because `eslint_d` was not installed
      ---- Markdown was slow because `write-good` and `markdownlint`
      ---- was not installed
      return {
        b.formatting.prettier.with {
          filetypes = { 'html', 'json', 'yaml', 'markdown' },
        },
        b.formatting.stylua.with {
          args = {
            '--config-path',
            ttd.paths.HOME .. '/dotfiles/nvim/lua/stylua.toml',
            '-',
          },
        },
        b.formatting.prettier_d_slim,
        -- b.formatting.stylua,
        b.formatting.trim_whitespace.with { filetypes = { 'tmux', 'fish', 'teal' } },
        b.formatting.shfmt,
        b.diagnostics.write_good,
        b.diagnostics.markdownlint,
        b.diagnostics.shellcheck.with {
          filetypes = { 'zsh', 'sh', 'bash' },
        },
        -- b.code_actions.gitsigns,
      }
    end)(),
  },
  bashls = {},
  tsserver = {
    cmd = { 'typescript-language-server', '--stdio' },
    on_attach = function(client, bufnr)
      local ts_utils = require 'nvim-lsp-ts-utils'
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
  },
  cssls = { cmd = { 'css-languageserver', '--stdio' } },
  rnix = {},
  jsonls = { cmd = { 'vscode-json-languageserver', '--stdio' } },
  html = { cmd = { 'html-languageserver', '--stdio' } },
  clangd = {},
  clojure_lsp = { filetypes = { 'clojure', 'edn' } },
  sumneko_lua = require('lua-dev').setup {
    library = {
      vimruntime = true, -- Runtime path
      types = true, -- Full signature, docs and completion of vim.api, vim.treesitter, vim.lsp and others
      plugins = true, -- Installed opt or start plugins in packpath
      -- plugins = { 'nvim-treesitter', 'plenary.nvim', 'telescope.nvim' },
    },
    lspconfig = {
      cmd = { 'lua-language-server' },
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
  },
  vimls = {},
  tailwindcss = { cmd = { 'tailwind-lsp' } },
}

for server, config in pairs(servers) do
  if server ~= 'null-ls' then
    lspconfig[server].setup(vim.tbl_deep_extend('force', {
      on_attach = on_attach,
      capabilities = capabilities,
      flags = {
        debounce_text_changes = 150,
      },
    }, config))
    local cfg = lspconfig[server]
    if not (cfg and cfg.cmd and vim.fn.executable(cfg.cmd[1]) == 1) then
      u.error(server .. ': cmd not found: ' .. vim.inspect(cfg.cmd))
    end
  else
    require('null-ls').setup(vim.tbl_deep_extend('force', {
      on_attach = on_attach,
      capabilities = capabilities,
    }, config))
  end
end

-- Lsp keymaps
_G.reload_lsp = function()
  lsp.stop_client(vim.lsp.get_active_clients())
  cmd [[edit]]
end

_G.open_lsp_log = function()
  local path = vim.lsp.get_log_path()
  cmd('edit ' .. path)
end

u.command('LspLog', 'call v:lua.open_lsp_log()')
u.command('LspRestart', 'call v:lua.reload_lsp()')

u.map('n', '<Leader>li', '<Cmd>LspInfo<CR>', { nowait = true })
u.map('n', '<Leader>ls', '<Cmd>LspStart<CR>', { nowait = true })
u.map('n', '<Leader>ll', '<Cmd>LspLog<CR>', { nowait = true })
u.map('n', '<Leader>lr', '<Cmd>LspRestart<CR>', { nowait = true })
