local opt = vim.opt
local has = vim.fn.has
local cmd = vim.cmd
local paths = ttd.paths

-----------------------------------------------------------
-- GENERAL SETTINGS
-----------------------------------------------------------

opt.lazyredraw = true
opt.mouse = 'nv'
opt.modeline = true
opt.report = 0
opt.encoding = 'utf-8'
opt.fileencoding = 'utf-8'
opt.errorbells = false
opt.visualbell = false
opt.magic = true
opt.hidden = true
opt.fileformats = 'unix,mac,dos'
opt.virtualedit = 'block'
opt.synmaxcol = 2500
opt.formatoptions = {
  ['1'] = true,
  ['2'] = true, -- Use indent from 2nd line of a paragraph
  q = true, -- continue comments with gq"
  c = true, -- Auto-wrap comments using textwidth
  r = true, -- Continue comments when pressing Enter
  n = true, -- Recognize numbered lists
  t = false, -- Autowrap lines using text width value
  j = true, -- Remove a comment leader when joining lines.
  -- Only break if the line was not longer than 'textwidth' when the insert
  -- started and only at a white character that has been entered during the
  -- current insert command.
  l = true,
  v = true,
}
opt.viewoptions = 'folds,cursor,curdir,slash,unix'
opt.sessionoptions = 'curdir,help,tabpages,winsize'
opt.clipboard = 'unnamedplus'

-- Wildmenu
-- --------
opt.wildmenu = true
opt.wildignorecase = true
-- Ignore compiled files
opt.wildignore = opt.wildignore
  + {
    '.git',
    '.hg',
    '.svn',
    '*.o',
    '*.out',
    '*.jpg',
    '*.jpeg',
    '*.png',
    '*.gif',
    '*.zip',
    '*~',
    '**/tmp/**, *.DS_Store',
    '**/node_modules/**',
    '**/bower_modules/**',
    '*.pyc',
    '*pycache*',
  }
opt.wildoptions = 'pum'
opt.wildmode = { 'longest:full', 'full' }

-- ---------------
-- Vim directories
-- ---------------
opt.undofile = true
opt.swapfile = false
opt.backup = false
opt.history = 5000
opt.writebackup = false
opt.directory = paths.CACHE_DIR .. 'swag/'
opt.undodir = paths.CACHE_DIR .. 'undo/'
opt.backupdir = paths.CACHE_DIR .. 'backup/'
opt.viewdir = paths.CACHE_DIR .. 'view/'
opt.spellfile = paths.CACHE_DIR .. 'spell/en.uft-8.add'
opt.backupskip = {
  '/tmp/*',
  '$TMPDIR/*',
  '$TMP/*',
  '$TEMP/*',
  '*/shm/*',
  '/private/var/*',
  '.vault.vim',
}
opt.shada = { '!', "'1000", '<50', '@100', 's10', 'h' }

-- ----------------
-- Tabs and Indents
-- ----------------
opt.textwidth = 80
opt.expandtab = true -- Don't expand tabs to spaces
opt.tabstop = 2
opt.shiftwidth = 2 -- Number of spaces to use in auto(indent)
opt.softtabstop = -1 -- Automatically keeps in sync with shiftwidth
opt.smarttab = true -- Tab insert blanks according to 'shiftwidth'
opt.autoindent = true -- Use same indenting on new lines
opt.smartindent = true -- Smart autoindenting on new lines
opt.shiftround = true -- Round indent to multiple of 'shiftwidth'
opt.showbreak = '↳ '
opt.breakindentopt = 'shift:2,min:20'

-- ------
-- Timing
-- ------
opt.timeout = true
opt.ttimeout = true
opt.updatetime = 100
opt.timeoutlen = 350
opt.ttimeoutlen = 10
opt.redrawtime = 1500

-- ---------
-- Searching
-- ---------
opt.path = '.,**' -- Search from project root
opt.ignorecase = true
opt.smartcase = true
opt.infercase = true
opt.incsearch = true
opt.hlsearch = true
opt.wrapscan = true
opt.complete = '.,w,b,k'
opt.inccommand = 'nosplit'
opt.grepformat = '%f:%l:%c:%m'
opt.grepprg = 'rg --hidden --vimgrep --smart-case --'
opt.re = 2
-- --------
-- Behavior
-- --------
opt.wrap = true
opt.linebreak = true
opt.breakat = [[\ \	;:,!?]]
opt.startofline = false
opt.whichwrap = 'b,s,<,>,h,l,[,],~'
opt.splitright = true
opt.splitbelow = true
opt.eadirection = 'hor'
opt.backspace = 'indent,eol,start'
opt.switchbuf = 'useopen,uselast'
opt.diffopt = vim.opt.diffopt
  + {
    'vertical',
    'iwhite',
    'hiddenoff',
    'foldcolumn:0',
    'context:4',
    'algorithm:histogram',
    'indent-heuristic',
  }
opt.completeopt = { 'menuone', 'noselect' }
opt.jumpoptions = 'stack'

-- --------------------
-- EDITOR UI APPEARANCE
-- --------------------
if has 'termguicolors' then
  cmd 'let &t_8f = "\\<Esc>[38;2;%lu;%lu;%lum"'
  cmd 'let &t_8b = "\\<Esc>[48;2;%lu;%lu;%lum"'
  opt.termguicolors = true
end
opt.autochdir = false
opt.shortmess = {
  t = true, -- truncate file messages at start
  A = true, -- ignore annoying swap file messages
  o = true, -- file-read message overwrites previous
  O = true, -- file-read message overwrites previous
  T = true, -- truncate non-file messages in middle
  f = true, -- (file x of x) instead of just (x of x
  F = true, -- Don't give file info when editing a file, NOTE: this breaks autocommand messages
  s = true,
  c = true,
  W = true, -- Dont show [w] or written when writing
}
vim.opt_global = 0
-- opt.scrolloff    = 0
-- vim.opt_local.so = 0
opt.sidescrolloff = 5
opt.cmdheight = 1
opt.showmode = false
opt.showtabline = 2
opt.ruler = false
opt.pumblend = 0
opt.winblend = 0
opt.pumheight = 15
opt.winwidth = 30
opt.winminwidth = 10
opt.winminheight = 0
opt.helpheight = 12
opt.previewheight = 12
opt.showcmd = false
opt.cmdwinheight = 2
opt.equalalways = false
opt.laststatus = 2
opt.display = 'lastline'
opt.number = true
opt.list = true
-- TreeSitter folding
-- Disable for now because slowness
opt.foldmethod = 'expr'
opt.foldexpr = 'nvim_treesitter#foldexpr()'
opt.foldenable = false
opt.foldtext = 'v:lua.folds()'
opt.foldopen = vim.opt.foldopen + 'search'
opt.foldlevelstart = 6
opt.cursorline = true
opt.cursorcolumn = false
opt.signcolumn = 'yes'
opt.conceallevel = 2
opt.concealcursor = 'niv'
opt.showmatch = true
opt.matchtime = 2
opt.linespace = 0
opt.joinspaces = false
opt.autoread = true

-- UI symbols
-- Icons:  ▏│ ¦ ╎ ┆ ⋮ ⦙ ┊ 
opt.listchars = {
  tab = '»·',
  nbsp = '+',
  trail = '·',
  extends = '→',
  precedes = '←',
}
opt.fillchars = {
  vert = '▕',
  fold = '·',
  diff = '',
  msgsep = '‾',
  eob = ' ',
  foldopen = '▾',
  foldsep = '│',
  foldclose = '▸',
}
