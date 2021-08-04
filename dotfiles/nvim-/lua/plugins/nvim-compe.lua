-- NVIM-COMPE PLUGIN
-- -----------------
local u = require 'core.utils'
local map = u.map

require('compe').setup {
  enabled = true,
  autocomplete = true,
  debug = false,
  min_length = 1,
  preselect = 'always',
  -- allow_prefix_unmatch = false,
  throttle_time = 80,
  source_timeout = 200,
  -- resolve_timeout = 800,
  incomplete_delay = 400,
  max_abbr_width = 100,
  max_kind_width = 100,
  max_menu_width = 100,
  documentation = {
    border = { '┌', '─', '┐', '│', '┘', '─', '└', '│' },
  },
  source = {
    path = true,
    buffer = true,
    calc = true,
    vsnip = true,
    nvim_lsp = true,
    nvim_lua = true,
    spell = false,
    tags = true,
    snippets_nvim = false,
  },
}

local function check_back_space()
  local col = vim.fn.col '.' - 1
  if col == 0 or vim.fn.getline('.'):sub(col, col):match '%s' then
    return true
  else
    return false
  end
end

--- Move to prev/next item in completion menuone
--- Jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return u.t '<C-n>'
  elseif vim.fn.call('vsnip#available', { 1 }) == 1 then
    return u.t '<Plug>(vsnip-expand-or-jump)'
  elseif check_back_space() then
    return u.t '<Tab>'
  else
    return vim.fn['compe#complete']()
  end
end

_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return u.t '<C-p>'
  elseif vim.fn.call('vsnip#jumpable', { -1 }) == 1 then
    return u.t '<Plug>(vsnip-jump-prev)'
  else
    return u.t '<S-Tab>'
  end
end

map('i', '<TAB>', 'v:lua.tab_complete()', { noremap = false, expr = true })
map('i', '<S-TAB>', 'v:lua.s_tab_complete()', { noremap = false, expr = true })
map('s', '<TAB>', 'v:lua.tab_complete()', { noremap = false, expr = true })
map('s', '<S-TAB>', 'v:lua.s_tab_complete()', { noremap = false, expr = true })
map('i', '<C-Space>', [[compe#complete()]], { expr = true })
-- map('i', '<CR>', [[compe#confirm('<CR>')]], {expr = true})
map('i', '<C-e>', [[compe#close('<C-e>')]], { expr = true })
-- map('i', '<C-f>', [[compe#scroll({'delta': +4 })]], { expr = true})
-- map('i', '<C-d>', [[compe#scroll({'delta': -4 })]], { expr = true})
--
vim.cmd [[autocmd User CompeConfirmDone silent! lua vim.lsp.buf.signature_help()]]
