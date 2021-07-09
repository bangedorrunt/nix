local lspconfig = require 'lspconfig'
local M = {}

-- local luafmt = { formatCommand = "lua-format -i", formatStdin = true }

local stylua = { formatCommand = 'stylua -', formatStdin = true }
local selene = {
  lintComman = 'selene --display-style quiet -',
  lintIgnoreExitCode = true,
  lintStdin = true,
  lintFormats = { '%f:%l:%c: %tarning%m', '%f:%l:%c: %tarning%m' },
}

local prettierLocal = {
  formatCommand = './node_modules/.bin/prettier --stdin --stdin-filepath ${INPUT}',
  formatStdin = true,
}

local prettierGlobal = {
  formatCommand = 'prettier --stdin --stdin-filepath ${INPUT}',
  formatStdin = true,
}

local eslint = {
  lintCommand = 'eslint_d -f visualstudio --stdin --stdin-filename ${INPUT}',
  lintIgnoreExitCode = true,
  lintStdin = true,
  lintFormats = { '%f(%l,%c): %tarning %m', '%f(%l,%c): %trror %m' },
}

local shellcheck = {
  lintCommand = 'shellcheck -f gcc -x -',
  lintStdin = true,
  lintFormats = {
    '%f=%l:%c: %trror: %m',
    '%f=%l:%c: %tarning: %m',
    '%f=%l:%c: %tote: %m',
  },
}

local markdownlint = {
  lintCommand = 'markdownlint -s',
  lintStdin = true,
  lintFormats = { '%f:%l:%c %m' },
}

local fish = { formatCommand = 'fish_indent', formatStdin = true }

local eslintPrettier = { prettierLocal, eslint }

M.setup = function(on_attach, capabilities)
  lspconfig.efm.setup {
    on_attach = function(client)
      on_attach(client)
      vim.opt_local.omnifunc = 'v:lua.vim.lsp.omnifunc'
    end,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    },
    init_options = { documentFormatting = true },
    settings = {
      rootMarkers = { 'package.json', '.git' },
      languages = {
        -- lua = { selene },
        lua = { stylua },
        -- typescript = { prettierLocal },
        -- javascript = eslintPrettier,
        -- typescriptreact = eslintPrettier,
        -- javascriptreact = eslintPrettier,
        -- ["typescript.tsx"] = eslintPrettier,
        -- ["javascript.tsx"] = eslintPrettier,
        -- yaml = { prettierLocal },
        -- json = { prettierGlobal },
        -- html = { prettierLocal },
        -- scss = { prettierLocal },
        -- css = { prettierLocal },
        -- markdown = { prettierLocal, markdownlint },
        -- sh = { shellcheck },
        fish = { fish },
      },
    },
    filetypes = { 'lua' },
  }
end
return M
