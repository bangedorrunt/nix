local cmd = vim.cmd

-- Buffs augroup
cmd [[
  aug Buffs
  au!
  aug END
  ]]

-- Wins augroup
cmd [[
  aug Wins
  au!
  aug END
  ]]

-- Filetype augroup
cmd [[
  aug FT
  au!
  aug END
  ]]

-- Smart `q` close windows
cmd [[au FileType help,startuptime,qf,lspinfo nnoremap <buffer><silent> q :close<CR>]]
cmd [[au FileType man nnoremap <buffer><silent> q :quit<CR>]]

-- Buffer

cmd [[au Buffs BufWritePost plugins.lua source <afile> | PackerCompile]]
cmd [[
  au Buffs BufWritePre /tmp/* setlocal noundofile
  au Buffs BufWritePre COMMIT_EDITMSG setlocal noundofile
  au Buffs BufWritePre MERGE_MSG setlocal noundofile
  au Buffs BufWritePre *.tmp setlocal noundofile
  au Buffs BufWritePre *.bak setlocal noundofile
  ]]

-- Show cursor line only in active window
cmd [[
  au Wins InsertLeave,WinEnter * set cursorline
  au Wins InsertEnter,WinLeave * set nocursorline
  ]]

-- Go to last loc when opening a buffer
cmd [[au Buffs BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif]]

-- Check if we need to reload the file when it changed
cmd 'au FocusGained * :checktime'

-- Highlight on yank
cmd 'au TextYankPost * lua vim.highlight.on_yank {timeout = 200}'

-- Filetype detect
cmd [[au FT BufRead,BufNewFile *.fish setfiletype fish]]
cmd [[au FT BufRead,BufNewFile *.nix setfiletype nix]]

-- Equalize window dimensions when resizing vim window
cmd [[au VimResized * tabdo wincmd=]]

-- Force write shada on leaving nvim
cmd [[au Wins WinLeave * if has('nvim') | wshada! | else | wviminfo! | endif]]

cmd [[
  aug Theme
    au!
    au ColorScheme * hi! NvimInternalError guibg=None | hi! RedrawDebugClear guibg=None | hi! RedrawDebugNormal guibg=None | hi! RedrawDebugComposed guibg=None | hi! RedrawDebugRecompose guibg=None | hi! IncSearch guibg=#dfccd4
  aug END
  ]]
