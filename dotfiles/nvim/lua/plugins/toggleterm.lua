-- toggleterm plugin
-- -----------------
require('toggleterm').setup {
  size = 20,
  hide_numbers = true,
  open_mapping = [[<C-t>]],
  shade_filetypes = {},
  shade_terminals = false,
  shading_factor = 0.3, -- The degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  start_in_insert = true,
  persist_size = true,
  direction = 'horizontal',
}

-- require('core.utils').map('n', '<C-`>', '<Cmd>exe v:count1 . "ToggleTerm"<CR>')
-- require('core.utils').map('i', '<C-`>', '<ESC><Cmd>exe v:count1 . "ToggleTerm"<CR>')
