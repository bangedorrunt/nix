local null_ls = require 'null-ls'
local b = null_ls.builtins

-- NOTE: When you experience any lag or unresponsive with Lsp,
---- make sure respective sources are installed
---- In my case:
---- Typescript was slow because `eslint_d` was not installed
---- Markdown was slow because `write-good` and `markdownlint`
---- was not installed
local sources = {
  b.formatting.prettier.with {
    filetypes = { 'html', 'json', 'yaml', 'markdown' },
  },
  b.formatting.stylua.with {
    command = ttd.paths.HOME .. '/.cargo/bin/stylua',
    args = {
      '--config-path',
      ttd.paths.HOME .. '/dotfiles/nvim/lua/stylua.toml',
      '-',
    },
  },
  b.formatting.prettier_d_slim,
  -- b.formatting.stylua,
  b.formatting.trim_whitespace.with { filetypes = { 'tmux', 'fish', 'teal' } },
  b.formatting.shfmt,
  b.diagnostics.write_good,
  b.diagnostics.markdownlint,
  b.diagnostics.shellcheck.with {
    filetypes = { 'zsh', 'sh', 'bash' },
  },
  b.code_actions.gitsigns,
}

local M = {}
M.setup = function(on_attach)
  null_ls.setup {
    -- debug = true,
    on_attach = on_attach,
    sources = sources,
  }
end
return M
