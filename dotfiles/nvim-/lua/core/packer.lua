local fn, api, uv, cmd = vim.fn, vim.api, vim.loop, vim.cmd
local paths = ttd.paths
local state = uv.fs_stat(paths.PACKER_DIR)

local M = {}

M.bootstrap = function()
  if not state then
    vim.notify 'Downloading packer.nvim...'
    local download_packer = '!git clone https://github.com/wbthomason/packer.nvim ' .. paths.PACKER_DIR
    api.nvim_command(download_packer)
    uv.fs_mkdir(paths.DATA_DIR .. 'lua', 511, function()
      assert 'Make compile path dir failed'
    end)
    cmd [[packadd packer.nvim]]
    M.load_plugins()
    require('packer').install()
  end

  cmd [[packadd packer.nvim]]
end

M.load_plugins = function()
  require 'plugins'
end

-- Packer doesn't support autoload from custom path, so
-- I gotta do it manually
M.load_compile = function()
  if fn.filereadable(paths.PACKER_COMPILED_PATH) == 1 then
    require 'packer_compiled'
  else
    assert 'Missing packer compile file Run PackerCompile Or PackerInstall to fix'
  end
end

return M
