(import-macros {: opt} :core.macros)

;;;; RENDERING
;; (opt background :light)

;;;; UI
(opt lz)
(opt number)
(opt relativenumber)
(opt termguicolors)
(opt cursorline)
(opt report 0)
(opt visualbell false)
(opt errorbells false)
(opt mouse :a)
(opt showmatch)
(opt matchtime 2)
(opt shortmess :filnxtToOFc)
(opt pumheight 15)
(opt winblend 0)
(opt winwidth 30)
(opt winminwidth 10)
(opt winminheight 0)
(opt helpheight 12)
(opt previewheight 12)
(opt cmdwinheight 12)
(opt cmdheight 0)
(opt conceallevel 2)
(opt concealcursor :nc)
(opt signcolumn "yes")
(opt colorcolumn :80)
(opt showmode false)
(opt laststatus 2)
(opt expandtab) ;; use space instead of tab
(opt tabstop 2)
(opt shiftwidth 0) ;; in favour of tabstop
(opt softtabstop -1)
(opt smartindent)
(opt breakindent) ;; maintain indentation on break
(opt showbreak " ")
(opt breakindentopt "shift:2")
;; Invisibles
(opt list)
(opt listchars {:tab "»·"
                :nbsp "+"
                :trail "·"
                :extends ""
                :precedes ""})

(opt fillchars {:vert ""
                :fold "·"
                :diff ""
                :msgsep "‾"
                :eob " "
                :foldopen ""
                :foldsep ""
                :foldclose ""})

;;;; BEHAVIOUR
(opt noautochdir)
(opt magic)
(opt hidden)
(opt scrolloff 10)
;; See: https://stackoverflow.com/a/50415982
(opt wrap)
(opt wrapmargin 0)
(opt whichwrap "b,s,<,>,h,l,[,],~")
(opt textwidth 80)
;; (opt columns 80)
(opt linebreak)

(opt virtualedit :block)
(opt fileformats [:unix :mac :dos])
(opt clipboard :unnamedplus)
(opt completeopt [:menu :menuone :preview :noinsert])
(opt diffopt+ [:vertical
               :iwhite
               :hiddenoff
               "foldcolumn:0"
               "context:4"
               "algorithm:histogram"
               :indent-heuristic])
(opt foldlevel 999) ;; Unfold them all
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
(opt updatetime 50)
(opt timeoutlen 750) ;; give me more time to do key sequences
(opt ttimeoutlen 50) ;; make escape more responsive
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
