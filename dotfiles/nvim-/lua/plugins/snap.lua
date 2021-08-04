-- SNAP PLUGIN
-- -----------
local snap = require 'snap'
local file = snap.config.file:with {
  -- reverse = true,
  suffix = ' ❯',
  prompt = 'Snap',
  consumer = 'fzy',
  -- SEE: https://github.com/camspiers/snap/issues/6#issuecomment-864893478
  -- SEE: https://github.com/camspiers/snap/pull/42#issuecomment-868984364
  args = { '--hidden', '--follow', '--glob', '!{.git,node_modules}/**' },
  layout = snap.get('layout').bottom,
}
local vimgrep = snap.config.vimgrep:with {
  -- reverse = true,
  suffix = ' ❯',
  prompt = 'RG',
  limit = 50000,
  args = { '--hidden', '--follow', '--glob', '!{.git,node_modules}/**' },
  layout = snap.get('layout').bottom,
}

local neovim = snap.config.file:with {
  -- reverse = true,
  suffix = ' ❯',
  prompt = 'Neovim Config',
  consumer = 'fzy',
  layout = snap.get('layout').bottom,
  combine = {
    snap.get('producer.ripgrep.file').args({}, os.getenv 'HOME' .. '/dotfiles/nvim-lua'),
    snap.get('producer.ripgrep.file').args({}, os.getenv 'HOME' .. '/dotfiles/nvim'),
  },
}
-- SEE: https://github.com/camspiers/snap/issues/26#issuecomment-864357557
-- Use `try` consumer
snap.maps {
  { '<Leader><Leader>', file { try = { 'git.file', 'ripgrep.file' } }, { 'Files' } },
  { '<Leader>;', vimgrep {}, { 'RG' } },
  { '<Leader>0', neovim {}, { 'Neovim_Config' } },
  {
    '<Leader>9',
    file {
      prompt = 'Dotfiles',
      try = {
        snap.get('producer.ripgrep.file').args(
          { '--hidden', '--follow', '--glob', '!{.git,node_modules}/**' },
          os.getenv 'HOME' .. '/dotfiles'
        ),
      },
    },
    { 'Dotfiles' },
  },
  { '<Leader>bb', file { producer = 'vim.buffer' }, { 'Buffers' } },
}

vim.cmd [[
  hi! link SnapSelect DiffAdd 
  hi! SnapPosition guibg=None ctermbg=None ctermfg=Red guifg=Red gui=Bold 
  hi! SnapBorder guifg=#e1e2e7
]]
