"---------------------------------------------------------
" GENERAL SETTINGS
"---------------------------------------------------------

set lazyredraw
set mouse=nv                 " Disable mouse in command-line mode
set modeline                 " Automatically setting options from modelines
set report=0                 " Don't report on line changes
set errorbells               " Trigger bell on error
set visualbell               " Use visual bell instead of beeping
set hidden                   " Hide buffers when abandoned instead of unload
set fileformats=unix,mac,dos " Use unix as the standard file type
set magic                    " For regular expressions turn magic on
set path+=**                 " Directories to search when using gf and friends
set isfname-==               " Remove =, detects filename in var=/foo/bar
set virtualedit=block        " Position cursor anywhere in visual block
set synmaxcol=2500           " Don't syntax highlight long lines
set formatoptions+=1         " Don't break lines after a one-letter word
" set formatoptions-=t       " Don't auto-wrap text
set formatoptions-=o         " Disable comment-continuation (normal 'o'/'o')
if has('patch-7.3.541')
	set formatoptions+=j       " Remove comment leader when joining lines
endif

if has('vim_starting')
	set encoding=utf-8
	scriptencoding utf-8
endif

" What to save for views:
set viewoptions=folds,cursor,curdir,slash,unix
set sessionoptions=curdir,help,tabpages,winsize

" What to save in sessions:
set sessionoptions-=blank    " Don't wanna see blank/empty windows
set sessionoptions-=options
set sessionoptions-=globals
set sessionoptions-=folds
set sessionoptions-=help
set sessionoptions-=tabpages " Don't see tabpages in session
set sessionoptions+=buffers  " Save all buffers in session

" macOS clipboard settings for faster init
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

" Wildmenu
" --------
if has('wildmenu')
	if ! has('nvim')
		set nowildmenu
		set wildmode=list:longest,full
	endif
	set wildignorecase
	set wildignore+=.git,.hg,.svn,.stversions,*.pyc,*.spl,*.o,*.out,*~,%*
	set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.zip,**/tmp/**,*.DS_Store
	set wildignore+=**/node_modules/**,**/bower_modules/**,*/.sass-cache/*
	set wildignore+=__pycache__,*.egg-info,.pytest_cache,.mypy_cache/**
endif


" ---------------
" Vim directories
" ---------------
set undofile swapfile nobackup
set nospell

" History saving
set history=5000

set directory=$CACHE_PATH/swap//,$CACHE_PATH,~/tmp,/var/tmp,/tmp
set undodir=$CACHE_PATH/undo//,$CACHE_PATH,~/tmp,/var/tmp,/tmp
set backupdir=$CACHE_PATH/backup/,$CACHE_PATH,~/tmp,/var/tmp,/tmp
set viewdir=$CACHE_PATH/view/
set spellfile=$NVIM_PATH/spell/en.utf-8.add

if has('nvim') && ! has('win32') && ! has('win64')

  " shada
  "  ' - maximum number of previously edited files marks
  "  < - maximum number of lines saved for each register
  "  @ - maximum number of items in the input-line history to be
  "  s - maximum size of an item contents in kib
  "  h - disable the effect of 'hlsearch' when loading the shada
	set shada=!,'300,<50,@100,s10,h
else
	set viminfo='300,<10,@50,h,n$DATA_PATH/viminfo
endif

augroup USER_PERSISTENT_UNDO
	autocmd!
	au BufWritePre /tmp/*          setlocal noundofile
	au BufWritePre COMMIT_EDITMSG  setlocal noundofile
	au BufWritePre MERGE_MSG       setlocal noundofile
	au BufWritePre *.tmp           setlocal noundofile
	au BufWritePre *.bak           setlocal noundofile
augroup END

" If sudo, disable vim swap/backup/undo/shada/viminfo writing
if $SUDO_USER !=# '' && $USER !=# $SUDO_USER
		\ && $HOME !=# expand('~'.$USER, 1)
		\ && $HOME ==# expand('~'.$SUDO_USER, 1)

	set noswapfile
	set nobackup
	set nowritebackup
	set noundofile
	if has('nvim')
		set shada="NONE"
	else
		set viminfo="NONE"
	endif
endif

" Secure sensitive information, disable backup files in temp directories
if exists('&backupskip')
	set backupskip+=/tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*,/private/var/*
	set backupskip+=.vault.vim
endif

" Disable swap/undo/viminfo files in temp directories or shm
augroup USER_SECURE
	autocmd!
	silent! autocmd BufNewFile,BufReadPre
		\ /tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*,/private/var/*,.vault.vim
		\ setlocal noswapfile noundofile
		\ | set nobackup nowritebackup
		\ | if has('nvim') | set shada= | else | set viminfo= | endif
augroup END

" ----------------
" Tabs and Indents
" ----------------
set textwidth=80    " Text width maximum chars before wrapping
set expandtab				" Don't expand tabs to spaces
set tabstop=2       " The number of spaces a tab is
set shiftwidth=2    " Number of spaces to use in auto(indent)
set softtabstop=-1  " Automatically keeps in sync with shiftwidth
set smarttab        " Tab insert blanks according to 'shiftwidth'
set autoindent      " Use same indenting on new lines
set smartindent     " Smart autoindenting on new lines
set shiftround      " Round indent to multiple of 'shiftwidth'

if exists('&breakindent')
	set breakindentopt=shift:2,min:20
endif

" ------
" Timing
" ------
set timeout ttimeout
set timeoutlen=350   " Time out on mappings
set ttimeoutlen=50   " Time out on key codes
set updatetime=500   " Idle time to write swap and trigger cursorhold
set redrawtime=2000  " Time in milliseconds for stopping display redraw

" ---------
" Searching
" ---------
set ignorecase    " Search ignoring case
set smartcase     " Keep case when searching with *
set infercase     " Adjust case in insert completion mode
set incsearch     " Incremental search
set wrapscan      " Searches wrap around the end of the file

set complete=.,w,b,k  " c-n completion: scan buffers, windows and dictionary

if exists('+inccommand')
	set inccommand=nosplit
endif

if executable('rg')
	set grepformat=%f:%l:%c:%m
	let &grepprg =
		\ 'rg --hidden --vimgrep' . (&smartcase ? ' --smart-case' : '') . ' --'
elseif executable('ag')
	set grepformat=%f:%l:%c:%m
	let &grepprg =
		\ 'ag --hidden --vimgrep' . (&smartcase ? ' --smart-case' : '') . ' --'
endif

" --------
" behavior
" --------
set wrap												" Wrap by default
set linebreak                   " Break long lines at 'breakat'
set breakat=\ \	;:,!?           " Long lines break chars
set nostartofline               " Cursor in same column for few commands
set whichwrap+=h,l,<,>,[,],~    " Move to following line on certain keys
set sb spr											" Splits open bottom right
set switchbuf=useopen           " Look for matching window buffers first
set backspace=indent,eol,start  " Intuitive backspacing in insert mode
set diffopt=filler,iwhite       " Diff mode: show fillers, ignore whitespace
set completeopt=menu,menuone    " Always show menu, even for one item

if has('patch-7.4.775')
	set completeopt+=noselect     " Do not select a match in the menu
endif

if exists('+completepopup')
	set completeopt+=popup
	set completepopup=height:4,width:60,highlight:InfoPopup
endif

if has('patch-7.4.775')
	" Do not insert any text for a match until the user selects from menu
	set completeopt+=noinsert
endif

if has('patch-8.1.0360') || has('nvim-0.5')
	set diffopt+=internal,algorithm:patience
	" set diffopt=indent-heuristic,algorithm:patience
endif

" Use the new Neovim :h jumplist-stack
if has('nvim-0.5')
	set jumpoptions=stack
endif

" --------------------
" EDITOR UI APPEARANCE
" --------------------
set shortmess=aoOTI     " Shorten messages and don't show intro
set scrolloff=2         " Keep at least 2 lines above/below
set sidescrolloff=5     " Keep at least 5 lines left/right
set noruler             " Disable default status ruler
set list                " Show hidden characters
set nu
set showtabline=2       " Always show the tabs line
set winwidth=30         " Minimum width for active window
set winminwidth=10      " Minimum width for inactive windows
" set winheight=4       " Minimum height for active window
" set winminheight=4    " Minimum height for inactive window
set pumheight=15        " Pop-up menu's line height
set helpheight=12       " Minimum help window height
set previewheight=12    " Completion preview height

set showcmd             " Show command in status line
set cmdheight=1         " Height of the command line
set cmdwinheight=1      " Command-line lines
set noequalalways       " Don't resize windows on split or close
" set colorcolumn=+0    " Column highlight at textwidth's max character-limit
set laststatus=2        " Always show a status line
set display=lastline

if has('folding') && has('vim_starting')
	set foldenable
	set foldmethod=indent
	set foldlevelstart=99
endif

if has('nvim-0.4')
	set signcolumn=auto:1
elseif exists('&signcolumn')
	set signcolumn=auto
endif


" UI symbols
" Icons:  ▏│ ¦ ╎ ┆ ⋮ ⦙ ┊ 
let &showbreak='↳  '
set listchars=tab:\▏\ ,extends:⟫,precedes:⟪,nbsp:␣,trail:·
set fillchars=foldopen:O,foldclose:x
set fillchars=vert:▉,fold:─

if has('nvim')
  set fcs=eob:\     " Remove tilde background
  let &fcs='eob: '  " Remove whitespace of tilde
endif

if has('patch-7.4.314')
	" Do not display completion messages
	set shortmess+=c
endif

if has('patch-7.4.1570')
	" Do not display message when editing files
	set shortmess+=F
endif

" if exists('+previewpopup')
"		set previewpopup=height:10,width:60
" endif

" Pseudo-transparency for completion menu and floating windows
if has('termguicolors') && &termguicolors
	if exists('&pumblend')
		set pumblend=10
	endif
	if exists('&winblend')
		set winblend=10
	endif
endif
