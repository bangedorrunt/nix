-- Declare global namespace
_G.__store_global_callbacks = __store_global_callbacks or {}
_G.store = { _store = __store_global_callbacks }

require('hotpot').setup {
  provide_require_fennel = true,
  enable_hotpot_diagnostics = true,
  modules = { correlate = true },
  compiler = {
    macros = { env = '_COMPILER', compilerEnv = _G, allowedGlobals = false },
  },
}
require 'core'
