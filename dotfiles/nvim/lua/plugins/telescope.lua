-- TELESCOPE PLUGIN
-- ----------------
if not packer_plugins['plenary.nvim'].loaded then
  vim.cmd [[packadd plenary.nvim]]
  vim.cmd [[packadd popup.nvim]]
  vim.cmd [[packadd telescope-fzf-native.nvim]]
end
local map = require('core.utils').map
local M = {}

-- SEE: https://github.com/folke/dot/blob/master/config/nvim/lua/config/telescope.lua#L70
M.project_files = function(opts)
  opts = opts or {}
  local _git_pwd = vim.fn.systemlist('git rev-parse --show-toplevel')[1]
  if vim.v.shell_error ~= 0 then
    local client = vim.lsp.get_active_clients()[1]
    if client then
      opts.cwd = client.config.root_dir
    end
    require('telescope.builtin').find_files(opts)
    return
  end
  require('telescope.builtin').git_files(opts)
end

require('telescope').setup {
  defaults = {
    prompt_prefix = '❯ ',
    selection_caret = '❯ ',
    winblend = 0,
    sorting_strategy = 'ascending',
    layout_strategy = 'bottom_pane',
    mappings = {
      i = {
        ['<ESC>'] = require('telescope.actions').close,
      },
    },
    layout_config = {
      prompt_position = 'bottom',
      height = 0.4,
    },
    file_ignore_patterns = { '.git', 'node_modules/.*', '.neuron/.*', 'alfred2/.*' },
    file_previewer = require('telescope.previewers').vim_buffer_cat.new,
    grep_previewer = require('telescope.previewers').vim_buffer_vimgrep.new,
    qflist_previewer = require('telescope.previewers').vim_buffer_qflist.new,
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = false,
      override_file_sorter = true,
    },
    fzf = {
      fuzzy = true, -- False will only do exact matching
      override_generic_sorter = false, -- Override the generic sorter
      override_file_sorter = true, -- Override the file sorter
      case_mode = 'smart_case', -- Or 'ignore_case' or 'respect_case'
      -- The default case_mode is 'smart_case'
    },
  },
}
require('telescope').load_extension 'fzf'
-- require('telescope').load_extension('session-lens')

map('n', '<Leader>ht', '<Cmd>Telescope<CR>', { nowait = true })
map('n', '<Leader><Leader>', '<Cmd>lua require"plugins.telescope".project_files()<CR>', { nowait = true })
map('n', '<Leader>;', '<Cmd>Telescope live_grep<CR>', { nowait = true })
map('n', '<Leader>bb', '<Cmd>Telescope buffers<CR>', { nowait = true })
map(
  'n',
  '<Leader>0',
  '<Cmd>lua require"telescope.builtin".find_files{cwd = os.getenv("HOME").."/nix/dotfiles/nvim"}<CR>',
  { nowait = true }
)
map(
  'n',
  '<Leader>9',
  '<Cmd>lua require"telescope.builtin".find_files{cwd = os.getenv("HOME").."/nix/dotfiles"}<CR>',
  { nowait = true }
)

return M
