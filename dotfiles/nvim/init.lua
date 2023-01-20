-- Declare global namespace
_G.__store_global_callbacks = __store_global_callbacks or {}
_G.store = { _store = __store_global_callbacks }

local plugins_path = vim.fn.stdpath 'data' .. '/lazy'
local lazy_path = plugins_path .. '/lazy.nvim'
local hotpot_path = plugins_path .. '/hotpot.nvim'

-- Add lazy.nvim to rtp
vim.opt.rtp:prepend(lazy_path)
-- Add hotpot.nvim to rtp
vim.opt.rtp:prepend(hotpot_path)

require('hotpot').setup {
  provide_require_fennel = true,
  enable_hotpot_diagnostics = true,
  compiler = {
    modules = { correlate = true },
    macros = { env = '_COMPILER', compilerEnv = _G, allowedGlobals = false },
  },
}
require 'core'
