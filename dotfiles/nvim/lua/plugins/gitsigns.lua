-- GITSIGNS PLUGIN
-- ---------------
require('gitsigns').setup {
  signs = {
    add = { hl = 'GitGutterAdd', text = '▋' },
    change = { hl = 'GitGutterChange', text = '▋' },
    delete = { hl = 'GitGutterDelete', text = '▋' },
    topdelete = { hl = 'GitGutterDeleteChange', text = '▔' },
    changedelete = { hl = 'GitGutterChange', text = '▎' },
  },
  keymaps = {
    -- Default keymap options
    noremap = true,
    buffer = true,

    ['n ]g'] = {
      expr = true,
      "&diff ? ']g' : '<Cmd>lua require\"gitsigns\".next_hunk()<CR>'",
    },
    ['n [g'] = {
      expr = true,
      "&diff ? '[g' : '<Cmd>lua require\"gitsigns\".prev_hunk()<CR>'",
    },

    ['n <leader>ghs'] = '<Cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['n <leader>ghu'] = '<Cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <leader>ghr'] = '<Cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['n <leader>ghp'] = '<Cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <leader>ghb'] = '<Cmd>lua require"gitsigns".blame_line()<CR>',

    -- Text objects
    ['o ih'] = ':<C-u>lua require"gitsigns".text_object()<CR>',
    ['x ih'] = ':<C-u>lua require"gitsigns".text_object()<CR>',
  },
}
