-- NVIM-TREESITTER PLUGIN
-- ----------------------
-- SEE: https://github.com/folke/dot/blob/master/config/nvim/lua/config/treesitter.lua
require('nvim-treesitter.configs').setup {
  -- NOTE: if neovim is unresponsive and slow
  -- don't use `comment` lang
  --
  -- This cause startup delay, use
  -- `TSInstall maintained` instead
  -- ensure_installed = "maintained",
  autopairs = { enable = true },
  autotag = { enable = true },
  highlight = {
    enable = true,
    use_languagetree = true,
  },
  context_commentstring = { enable = true },
  indent = { enable = false },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<C-n>',
      node_incremental = '<C-n>',
      scope_incremental = '<C-s>',
      node_decremental = '<C-r>',
    },
  },
  query_linter = {
    enable = true,
    use_virtual_text = true,
    lint_events = { 'BufWrite', 'CursorHold' },
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = { [']m'] = '@function.outer', [']]'] = '@class.outer' },
      goto_next_end = { [']M'] = '@function.outer', [']['] = '@class.outer' },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = { ['[M'] = '@function.outer', ['[]'] = '@class.outer' },
    },
    lsp_interop = {
      enable = true,
      peek_definition_code = {
        ['gD'] = '@function.outer',
      },
    },
  },
}
-- Add Markdown (deprecated due to #872)
-- local parser_config = require('nvim-treesitter.parsers').get_parser_configs()
-- parser_config.jsonc.used_by = 'json'
-- parser_config.markdown = {
--   install_info = {
--     url = 'https://github.com/ikatyang/tree-sitter-markdown',
--     files = { 'src/parser.c', 'src/scanner.cc' },
--   },
-- }
