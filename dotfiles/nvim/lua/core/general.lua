local home = os.getenv 'HOME'
local os_name = vim.loop.os_uname().sysname
local path_sep = os_name == 'Windows' and '\\' or '/'
local data_dir = string.format('%s/site/', vim.fn.stdpath 'data')

ttd = {
  signs = {
    error = ' ',
    warning = ' ',
    hint = ' ',
    information = ' ',
    prompt = '❯',
  },
  pallete = {
    dark = {
      tokyonight = '#1a1b26',
      monokaipro_spectrum = '#222222',
    },
    light = {
      tokyonight = '#e1e2e7',
      gruvbox = '#fbf1c7',
    },
  },
  paths = {
    IS_MAC = os_name == 'Darwin',
    IS_LINUX = os_name == 'Linux',
    IS_WINDOWS = os_name == 'Windows',
    PATH_SEP = path_sep,
    VIM_PATH = vim.fn.stdpath 'config',
    HOME = home,
    CACHE_DIR = home .. path_sep .. '.cache' .. path_sep .. 'nvim' .. path_sep,
    DATA_DIR = data_dir,
    PACKER_DIR = data_dir .. 'pack/packer/opt/packer.nvim',
    PACKER_COMPILED_PATH = data_dir .. '/lua/packer_compiled.lua',
  },
}

local paths = ttd.paths

-- Create cache dir and subs dir
local createdir = function()
  local nvim_data_dir = {
    paths.CACHE_DIR .. 'backup',
    paths.CACHE_DIR .. 'session',
    paths.CACHE_DIR .. 'swap',
    paths.CACHE_DIR .. 'tags',
    paths.CACHE_DIR .. 'undo',
  }
  -- There only check once that if CACHE_DIR exists
  -- Then I don't want to check subs dir exists
  if vim.fn.isdirectory(paths.CACHE_DIR) == 0 then
    os.execute('mkdir -p ' .. paths.CACHE_DIR)
    for _, v in pairs(nvim_data_dir) do
      if vim.fn.isdirectory(v) == 0 then
        os.execute('mkdir -p ' .. v)
      end
    end
  end
end

local disable_distribution_plugins = function()
  vim.g.loaded_gzip = 1
  vim.g.loaded_tar = 1
  vim.g.loaded_tarPlugin = 1
  vim.g.loaded_zip = 1
  vim.g.loaded_zipPlugin = 1
  vim.g.loaded_getscript = 1
  vim.g.loaded_getscriptPlugin = 1
  vim.g.loaded_vimball = 1
  vim.g.loaded_vimballPlugin = 1
  vim.g.loaded_matchit = 1
  vim.g.loaded_matchparen = 1
  vim.g.loaded_2html_plugin = 1
  vim.g.loaded_logiPat = 1
  vim.g.loaded_rrhelper = 1
  vim.g.loaded_netrw = 1
  vim.g.loaded_netrwPlugin = 1
  vim.g.loaded_netrwSettings = 1
  vim.g.loaded_netrwFileHandlers = 1
  vim.g.loaded_perl_provider = 0
  vim.g.loaded_python_provider = 0
  vim.g.loaded_ruby_provider = 0

  vim.g.python3_host_prog = '~/.pyenv/shims/python'
end

createdir()
disable_distribution_plugins()
