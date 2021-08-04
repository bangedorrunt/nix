-- NVIM-AUTOPAIRS PLUGIN
-- ---------------------
local Rule = require 'nvim-autopairs.rule'

require('nvim-autopairs').setup {
  check_ts = true,
  ts_config = {
    lua = { 'string' }, -- It will not add pair on that treesitter node
    javascript = { 'template_string' },
  },
  enable_check_bracket_line = false,
  ignored_next_char = '[%w%.]', -- Will ignore alphanumeric and `.` symbol
  fast_wrap = {},
}

local ts_conds = require 'nvim-autopairs.ts-conds'
-- Press % => %% is only inside comment or string
require('nvim-autopairs').add_rules {
  Rule('%', '%', 'lua'):with_pair(ts_conds.is_ts_node { 'string', 'comment' }),
  Rule('$', '$', 'lua'):with_pair(ts_conds.is_not_ts_node { 'function' }),
}

require('nvim-autopairs.completion.compe').setup {
  map_cr = true, --  Map <CR> on insert mode
  map_complete = true, -- It will auto insert `(` after select function or method item
}
