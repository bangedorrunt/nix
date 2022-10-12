-- Declare global namespace
--- Inspired by @tjdevries' astraunauta.nvim/ @TimUntersberger's config
--- store all callbacks in one global table so they are able to survive re-requiring this file
_G.__store_global_callbacks = __store_global_callbacks or {}
_G.store = { _store = __store_global_callbacks }

require('hotpot').setup {
  provide_require_fennel = true,
  enable_hotpot_diagnostics = true,
  compiler = {
    macros = { env = '_COMPILER', compilerEnv = _G, allowedGlobals = false },
  },
}

require 'core'
