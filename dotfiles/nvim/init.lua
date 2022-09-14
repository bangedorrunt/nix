-- Declare global namespace
--- Inspired by @tjdevries' astraunauta.nvim/ @TimUntersberger's config
--- store all callbacks in one global table so they are able to survive re-requiring this file
_G.__tdt_global_callbacks = __tdt_global_callbacks or {}
_G.__nvim_global_callbacks = __nvim_global_callbacks or {}
_G.tdt = { _store = __tdt_global_callbacks }
_G.nvim = { _store = __nvim_global_callbacks }

require('hotpot').setup {
  provide_require_fennel = true,
  enable_hotpot_diagnostics = false,
  compiler = {
    macros = { env = '_COMPILER', compilerEnv = _G, allowedGlobals = false },
  },
}

require 'core'
