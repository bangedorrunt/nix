-- Declare global namespace
--- Inspired by @tjdevries' astraunauta.nvim/ @TimUntersberger's config
--- store all callbacks in one global table so they are able to survive re-requiring this file
_G.__tdt_global_callbacks = __tdt_global_callbacks or {}
_G.__nvim_global_callbacks = __nvim_global_callbacks or {}

_G.tdt = {
  _store = __tdt_global_callbacks,
}

_G.nvim = {
  _store = __nvim_global_callbacks,
}

-- Enable Aniseed's automatic compilation and loading of Fennel source code.
-- Aniseed looks for this when it's loaded then loads the rest of your
-- configuration if it's set.
vim.g['aniseed#env'] = true

-- For nathom/filetype.nvim
vim.g.did_load_filetypes = 1
