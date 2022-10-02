-- Declare global namespace
--- Inspired by @tjdevries' astraunauta.nvim/ @TimUntersberger's config
--- store all callbacks in one global table so they are able to survive re-requiring this file
_G.__bangedorrunt_global_callbacks = __bangedorrunt_global_callbacks or {}
_G.bangedorrunt = { _store = __bangedorrunt_global_callbacks }

-- Temporarily disable syntax and filetype to improve startup time
vim.api.nvim_command 'syntax off'
vim.api.nvim_command 'filetype off'
vim.api.nvim_command 'filetype plugin indent off'

-- Temporarily disable Shada file to improve startup time
vim.opt.shadafile = 'NONE'

require('hotpot').setup {
  provide_require_fennel = true,
  enable_hotpot_diagnostics = true,
  compiler = {
    macros = { env = '_COMPILER', compilerEnv = _G, allowedGlobals = false },
  },
}

require 'core'
