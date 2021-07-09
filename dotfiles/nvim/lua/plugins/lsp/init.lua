-- REF: https://github.com/akinsho/dotfiles/blob/01ddf20de97e29cbada48a819f0f6d980c6b7abe/.config/nvim/lua/as/plugins/lspconfig.lua
local u = require 'core.utils'
local sumneko = require 'plugins.lsp.sumneko'
local null_ls = require 'plugins.lsp.null-ls'
local tsserver = require 'plugins.lsp.tsserver'
local tailwindcss = require 'plugins.lsp.tailwindcss'
local clojure_lsp = require 'plugins.lsp.clojure-lsp'

-- Prefer null-ls for now
-- local efm = require("plugins.lsp.efm")

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
tailwindcss.setup(on_attach, capabilities)
tsserver.setup(on_attach, capabilities)
sumneko.setup(on_attach, capabilities)
null_ls.setup(on_attach)
clojure_lsp.setup(on_attach, capabilities)
-- efm.setup(on_attach, capabilities)

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
