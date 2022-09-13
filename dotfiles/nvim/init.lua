-- Declare global namespace
--- Inspired by @tjdevries' astraunauta.nvim/ @TimUntersberger's config
--- store all callbacks in one global table so they are able to survive re-requiring this file
_G.__tdt_global_callbacks = __tdt_global_callbacks or {}
_G.__nvim_global_callbacks = __nvim_global_callbacks or {}
_G.tdt = { _store = __tdt_global_callbacks }
_G.nvim = { _store = __nvim_global_callbacks }

require('hotpot').setup {
  provide_require_fennel = true,
  enable_hotpot_diagnostics = true,
  compiler = {
    macros = { env = '_COMPILER', compilerEnv = _G, allowedGlobals = false },
  },
}
-- AOT compile
-- stylua: ignore
require('hotpot.api.make').build(
  vim.fn.stdpath 'config',
  { verbosity = 0 },
  '(.+)/fnl/(.+)',
  function(root, path, opts)
    local join_path = opts['join-path']
    if not string.match(path, 'macros%.fnl$') then
      return join_path(root, 'lua', path)
    else
      return nil
    end
  end)

require 'core'
