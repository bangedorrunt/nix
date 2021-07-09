-- KOMMENTARY PLUGIN
-- -----------------
local remap = require('core.utils').map

vim.g.kommentary_create_default_mappings = false

require('kommentary.config').configure_language('default', {
  prefer_single_line_comments = true,
  use_consistent_indentation = true,
  ignore_whitespace = true,
})

-- See: https://github.com/b3nj5m1n/kommentary/issues/20#issuecomment-774664395
remap('n', 'gcc', '<Plug>kommentary_line_default', { noremap = false })
remap('n', 'gc', '<Plug>kommentary_motion_default', { noremap = false })
remap('v', 'gc', '<Plug>kommentary_visual_default<C-c>', { noremap = false })
