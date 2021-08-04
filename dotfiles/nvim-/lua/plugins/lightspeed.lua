-- LIGHTSPEED PLUGIN
-- -----------------
local remap = require('core.utils').map
require('lightspeed').setup {
  jump_to_first_match = true,
  jump_on_partial_input_safety_timeout = 400,
  highlight_unique_chars = false,
  grey_out_search_area = true,
  match_only_the_start_of_same_char_seqs = true,
  limit_ft_matches = 20,
  full_inclusive_prefix_key = '<C-x>',
  -- By default, the values of these will be decided at runtime,
  -- based on `jump_to_first_match`
  labels = nil,
  cycle_group_fwd_key = nil,
  cycle_group_bwd_key = nil,
}

vim.cmd [[
  unmap s
  unmap S
]]

remap('n', '<Leader>jj', '<Plug>Lightspeed_s', { noremap = false })
remap('n', '<Leader>jJ', '<Plug>Lightspeed_S', { noremap = false })
remap('n', ';', '<Cmd>lua repeat_ft(false)<CR>', { noremap = true, silent = true })
remap('x', ';', '<Cmd>lua repeat_ft(false)<CR>', { noremap = true, silent = true })
remap('n', ',', '<Cmd>lua repeat_ft(true)<CR>', { noremap = true, silent = true })
remap('x', ',', '<Cmd>lua repeat_ft(true)<CR>', { noremap = true, silent = true })
