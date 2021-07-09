local map = require('core.utils').map

-- DEFAULT MAP
-- -----------
vim.g.mapleader = ' '

-- Disable SPC key
-- Note: `which-key` already implemented this
-- map('n', '<Space>', '<Nop>')

map('n', '<TAB>', '<Cmd>bn<CR>')
map('n', '<S-TAB>', '<Cmd>bp<CR>')

-- Break lines in normmal mode
map('n', 'o', 'o<ESC>', { noremap = false })
map('n', 'O', 'O<ESC>', { noremap = false })
-- SEE: https://dev.to/snawaz/comment/pa71
-- map('n', 'o', ':<C-u>call append(line("."),   repeat([""], v:count1))<CR>', {noremap = false})
-- map('n', 'O', ':<C-u>call append(line(".")-1,   repeat([""], v:count1))<CR>', {noremap = false})
-- Remove search highlights
map('n', '<F2>', '<Cmd>noh<CR>')
-- Vim map
map('n', 'Y', 'y$', { noremap = false })
map('n', '<C-h>', '<C-w>h', { noremap = false })
map('n', '<C-l>', '<C-w>l', { noremap = false })
map('n', '<C-j>', '<C-w>j', { noremap = false })
map('n', '<C-k>', '<C-w>k', { noremap = false })
map('n', '<A-[>', '<Cmd>vertical resize -5<CR>', { noremap = false })
map('n', '<A-]>', '<Cmd>vertical resize +5<CR>', { noremap = false })
-- Insert
map('i', '<C-w>', '<C-[>diwa')
map('i', '<C-h>', '<BS>')
map('i', '<C-d>', '<Del>')
map('i', '<C-u>', '<C-G>u<C-U>')
map('i', '<C-b>', '<Left>')
map('i', '<C-f>', '<Right>')
map('i', '<C-a>', '<ESC>^i')
map('i', '<C-j>', '<Esc>o')
map('i', '<C-k>', '<Esc>O')
map('i', '<C-s>', '<Cmd>w<CR>')
map('i', '<C-q>', '<Cmd>wq<CR>')
-- map('i', '<C-e>', [[pumvisible() ? '<C-e>' : '<End>']], { expr = true, silent = false })

-- Command line
map('c', '<C-b>', '<Left>')
map('c', '<C-f>', '<Right>')
map('c', '<C-a>', '<Home>')
map('c', '<C-e>', '<End>')
map('c', '<C-d>', '<Del>')
map('c', '<C-h>', '<BS>')
map('c', '<C-t>', [[<C-r>=expand('%:p:h') . '/' <CR>]])

-- Term
map('t', '<Esc><Esc>', [[<C-\><C-n>]])
map('t', '<C-j>', [[<C-\><C-n><C-w>j]])
map('t', '<C-h>', [[<C-\><C-n><C-w>h]])
map('t', '<C-k>', [[<C-\><C-n><C-w>k]])
map('t', '<C-l>', [[<C-\><C-n><C-w>l]])

-- ------------------------
-- Quit
-- ------------------------
map('n', '<Leader>qq', ':<C-u>confirm qa<CR>', { noremap = false })
map('n', '<Leader>qQ', ':<C-u>qa!<CR>', { noremap = false })
map('n', '<Leader>qs', ':<C-u>wq<CR>', { noremap = false })

-- ------------------------
-- FILE & BUFFER NAVIGATION
-- ------------------------

-- Save
map('n', '<Leader>fs', ':<C-u>w<CR>')
map('n', '<Leader>bs', ':<C-u>w<CR>')
-- Save all
map('n', '<Leader>fS', ':<C-u>wa<CR>')
map('n', '<Leader>bS', ':<C-u>wa<CR>')
-- Close
-- Smart way to close buffers without losing split windows
-- SEE: http://bit.ly/2heyMZ8
map('n', '<Leader>fd', '<Cmd>bp|bd #<CR>')
map('n', '<Leader>fc', '<Cmd>bp|bd #<CR>')
map('n', '<Leader>bd', '<Cmd>bp|bd #<CR>')
map('n', '<Leader>bc', '<Cmd>bp|bd #<CR>')
-- Create new file under current dir
map('n', '<Leader>fo', ':<C-u>e <C-r>=expand("%:p:h") . "/"<CR>', { silent = false })
map('n', '<Leader>bo', ':<C-u>e <C-r>=expand("%:p:h") . "/"<CR>', { silent = false })
-- Rename
map('n', '<Leader>fr', ':<C-u>Rename ', { silent = false })
map('n', '<Leader>br', ':<C-u>Rename ', { silent = false })
-- Move
map('n', '<Leader>fm', ':<C-u>Move ', { silent = false })
map('n', '<Leader>bm', ':<C-u>Move ', { silent = false })
-- Delete
map('n', '<Leader>fD', ':<C-u>Delete!<CR>')
map('n', '<Leader>bD', ':<C-u>Delete!<CR>')

map('n', '<Leader>bn', '<Cmd>bn<CR>')
map('n', '<Leader>bp', '<Cmd>bp<CR>')
map('n', '<Leader>fn', '<Cmd>bn<CR>')
map('n', '<Leader>fp', '<Cmd>bp<CR>')

-- ------------------------
-- HELP
-- ------------------------
-- Packer

map('n', '<Leader>hpu', '<Cmd>PackerUpdate<CR>', { nowait = true })
map('n', '<Leader>hpi', '<Cmd>PackerInstall<CR>', { nowait = true })
map('n', '<Leader>hpc', '<Cmd>PackerCompile<CR>', { nowait = true })
map('n', '<Leader>hps', '<Cmd>PackerSync<CR>', { nowait = true })
map('n', '<Leader>hpp', '<Cmd>PackerProfile<CR>', { nowait = true })

-- ------------------------
-- Project
-- ------------------------

-- map('n', '<Leader>pp', '<Cmd>lua require("session-lens").search_session()<CR>')
-- map('n', '<Leader>ps', '<Cmd>SaveSessiocope<CR>')
-- map('n', '<Leader>pd', '<Cmd>DeleteSession<CR>')
-- map('n', '<Leader>pr', '<Cmd>RestoreSession<CR>')

-- ------------------------
-- WINDOWS NAVIGATION
-- ------------------------
map('n', '<Leader>wj', '<C-w>j')
map('n', '<Leader>wk', '<C-w>k')
map('n', '<Leader>wh', '<C-w>h')
map('n', '<Leader>wl', '<C-w>l')
map('n', '<Leader>wJ', '<C-w>J')
map('n', '<Leader>wK', '<C-w>K')
map('n', '<Leader>wH', '<C-w>H')
map('n', '<Leader>wL', '<C-w>L')
map('n', '<Leader>wc', '<C-w>c')
map('n', '<Leader>ww', '<C-w>w')
map('n', '<Leader>w=', '<C-w>=')
map('n', '<Leader>ws', '<Cmd>sp<CR>')
map('n', '<Leader>wv', '<Cmd>vsplit<CR>')

-- Plugin vim-operator-surround
-- map('n', '<leader>xsa', '<Cmd><Plug>(operator-surround-append)<CR>', {noremap = false})
-- map('n', '<leader>xsd', '<Cmd><Plug>(operator-surround-delete)<CR>', {noremap = false})
-- map('n', '<leader>xsr', '<Cmd><Plug>(operator-surround-replace)<CR>', {noremap = false})

-- Plugin vim_niceblock
-- map('x', 'I', 'v:lua.enhance_nice_block("I")', {noremap = false, expr = true})
-- map('x', 'gI', 'v:lua.enhance_nice_block("gI")', {noremap = false, expr = true})
-- map('x', 'A', 'v:lua.enhance_nice_block("A")', {noremap = false, expr = true})
