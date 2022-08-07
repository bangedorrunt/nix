(module core.options
  {require-macros [core.macros]})

;;;; RENDERING
(opt background :light)

(if (has? :termguicolors)
    (do
      (vim.cmd "let &t_8f = '\\<Esc>[38;2;%lu;%lu;%lum'")
      (vim.cmd "let &t_8b = '\\<Esc>[48;2;%lu;%lu;%lum'")
      (opt termguicolors))
    nil)

;;;; UI
(opt lz)
(opt number)
(opt report 0)
(opt visualbell false)
(opt errorbells false)
(opt mouse :a)
(opt cursorline)
(opt showmatch)
(opt matchtime 2)
(opt shortmess :filnxtToOFc)
(opt pumblend 0)
(opt pumheight 15)
(opt winblend 0)
(opt winwidth 30)
(opt winminwidth 10)
(opt winminheight 0)
(opt helpheight 12)
(opt previewheight 12)
(opt cmdwinheight 12)
(opt conceallevel 2)
(opt concealcursor :nc)
(opt signcolumn :yes)
;; Statusline
(opt showmode false)
(opt laststatus 2)
(opt textwidth 80)
;; Let vim-sleuth handle indent options
;; (opt expandtab)
;; (opt shiftwidth 2)
(opt tabstop 2)
(opt softtabstop -1)
(opt smarttab)
;; (opt autoindent)
;; (opt smartindent)
;; (opt shiftround)
(opt showbreak "↳ ")
(opt breakindentopt "shift:2,min:20")
;; Invisibles
(opt list)
(opt listchars {:tab "»·"
                :nbsp "+"
                :trail "·"
                :extends "→"
                :precedes "←"})

(opt fillchars {:vert "▕"
                :fold "·"
                :diff ""
                :msgsep "‾"
                :eob " "
                :foldopen "▾"
                :foldsep "│"
                :foldclose "▸"})

;;;; BEHAVIOUR
(opt magic)
;; (opt hidden)
(opt scrolloff 10)
;; (opt autoread)
(opt wrap)
(opt whichwrap "b,s,<,>,h,l,[,],~")
(opt nolinebreak)
(opt virtualedit :block)
(opt fileformats "unix,mac,dos")
(opt clipboard :unnamedplus)
(opt completeopt [:menu :menuone :preview :noselect])
(opt diffopt+ [:vertical
               :iwhite
               :hiddenoff
               "foldcolumn:0"
               "context:4"
               "algorithm:histogram"
               :indent-heuristic])

(opt splitright)
(opt splitbelow)
(opt backspace "indent,eol,start")
(opt switchbuf "useopen,uselast")
(opt eadirection :hor)
(opt sessionoptions "curdir,help,tabpages,winsize")
(opt viewoptions "folds,cursor,curdir,slash,unix")
;; Wildmenu
(opt wildmenu)
(opt wildignorecase)
(opt wildignore+ [:.git
                  :.hg
                  :.svn
                  :*.o
                  :*.out
                  :*.jpg
                  :*.jpeg
                  :*.png
                  :*.gif
                  :*.zip
                  "*~"
                  "**/tmp/** *.DS_Store"
                  :**/node_modules/**
                  :**/bower_modules/**
                  :*.pyc
                  :*pycache*])

(opt wildoptions :pum)
(opt wildmode "longest:full,full")
;; Time
(opt timeout)
(opt ttimeout)
(opt updatetime 100)
(opt timeoutlen 350)
(opt ttimeoutlen 10)
(opt redrawtime 1500)
;; Search
(opt ignorecase)
(opt incsearch)
(opt hlsearch)
(opt smartcase)
(opt infercase)
(opt wrapscan)
(opt inccommand :nosplit)
(opt complete ".,w,b,k")
(opt grepformat "%f:%l:%c:%m")
(opt grepprg "rg --hidden --vimgrep --smart-case --")

;;;; VIM DIRECTORIES
(opt undofile)
(opt swapfile false)
(opt backup false)
(opt history 5000)
(opt writebackup false)
(opt directory (.. tdt.paths.STATE_PATH :/swag/))
(opt undodir (.. tdt.paths.STATE_PATH :/undo/))
(opt backupdir (.. tdt.paths.STATE_PATH :/backup/))
(opt viewdir (.. tdt.paths.STATE_PATH :/view/))
(opt spellfile (.. tdt.paths.STATE_PATH :/spell/en.uft-8.add))
(opt backupskip [:/tmp/*
                 :$TMPDIR/*
                 :$TMP/*
                 :$TEMP/*
                 :*/shm/*
                 :/private/var/*
                 :.vault.vim])
;; fnlfmt: skip
(opt shada ["!" "'1000" "<50" "@100" "s10" "h"])
