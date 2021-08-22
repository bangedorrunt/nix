-- Declare global namespace
--- Inspired by @tjdevries' astraunauta.nvim/ @TimUntersberger's config
--- store all callbacks in one global table so they are able to survive re-requiring this file
_G.__as_global_callbacks = __as_global_callbacks or {}
_G.tdt = {
  _store = __as_global_callbacks,
}

-- Enable Aniseed's automatic compilation and loading of Fennel source code.
-- Aniseed looks for this when it's loaded then loads the rest of your
-- configuration if it's set.
-- vim.g['aniseed#env'] = { module = 'init' }
vim.g['aniseed#env'] = true

-- Now head to fnl/init.fnl to continue your journey.
--
-- require 'hotpot'
-- require 'init'
