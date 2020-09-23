"" ---------------------
"" general settings
"" ---------------------

" neovim live substitution
" not supported as of v0.52
set inccommand=nopsplit
set ignorecase      " search ignoring case
set smartcase       " keep case when searching with *
set infercase       " adjust case in insert completion mode
set showmatch       " jump to matching bracket
set matchpairs+=<:> " add html brackets to pair matching
set matchtime=1     " tenths of a second to show the matching paren
" tabs and indents
set textwidth=80   " text width maximum chars before wrapping
set expandtab      " expand tabs to spaces.
set tabstop=2      " the number of spaces a tab is
set softtabstop=2  " while performing editing operations
set shiftwidth=2   " number of spaces to use in auto(indent)
set autoindent     " use same indenting on new lines
set smartindent    " smart autoindenting on new lines
set shiftround     " round indent to multiple of 'shiftwidth'

" macos clipboard settings for faster init
if has('mac')
  let g:clipboard = {
    \   'name': 'macOS-clipboard',
    \   'copy': {
    \      '+': 'pbcopy',
    \      '*': 'pbcopy',
    \    },
    \   'paste': {
    \      '+': 'pbpaste',
    \      '*': 'pbpaste',
    \   },
    \   'cache_enabled': 0,
    \ }
endif

if has('clipboard')
  set clipboard& clipboard+=unnamedplus
endif
