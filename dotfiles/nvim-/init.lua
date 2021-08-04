-- Declare global namespace
--- Inspired by @tjdevries' astraunauta.nvim/ @TimUntersberger's config
--- store all callbacks in one global table so they are able to survive re-requiring this file
_G.__as_global_callbacks = __as_global_callbacks or {}
_G.ttd = {
  _store = __as_global_callbacks,
}

-- Load Modules:
require 'core'
