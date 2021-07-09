-- HLSLENS PLUGIN
-- --------------
local cmd = vim.cmd

require('hlslens').setup {
  auto_enable = true,
  enable_incsearch = true,
  calm_down = false,
  nearest_only = false,
  nearest_float_when = 'auto',
  float_shadow_blend = 50,
  virt_priority = 100,
}

cmd [[com! HlSearchLensToggle lua require('hlslens').toggle()]]
