(import-macros {: set!} :core.macros)

;;;; RENDERING
;; (set! background :light)

;;;; UI
(set! lz)
(set! number)
(set! relativenumber)
(set! termguicolors)
(set! spell)
(set! cursorline)
(set! report 0)
(set! visualbell false)
(set! errorbells false)
(set! mouse :a)
(set! showmatch)
(set! matchtime 2)
(set! shortmess :filnxtToOFc)
(set! pumheight 15)
(set! winblend 0)
(set! winwidth 30)
(set! winminwidth 10)
(set! winminheight 0)
(set! helpheight 12)
(set! previewheight 12)
(set! cmdwinheight 12)
(set! cmdheight 0)

;; crashed nvim when using vim-fugitive
(set! conceallevel 2)
(set! concealcursor :nc)
(set! signcolumn :yes)
(set! colorcolumn :80)
(set! showmode false)
(set! laststatus 3)

;; global status
(set! expandtab)

;; use space instead of tab
(set! tabstop 2)
(set! shiftwidth 0)

;; in favour of tabstop
(set! softtabstop -1)
(set! smartindent)
(set! breakindent)

;; maintain indentation on break
(set! showbreak " ")
(set! breakindentopt "shift:2")
;; Invisibles
(set! list)

;; show invisible chars
(set! listchars {:tab "»·"
                 :nbsp "+"
                 :trail "·"
                 :extends ""
                 :precedes ""})

(set! fillchars {:vert ""
                 :fold "·"
                 :diff ""
                 :msgsep "‾"
                 :eob " "
                 :foldopen ""
                 :foldsep ""
                 :foldclose ""})

;;;; BEHAVIOUR
(set! noautochdir)
(set! magic)
(set! hidden)
(set! scrolloff 10)
;; See: https://stackoverflow.com/a/50415982
(set! wrap)
(set! wrapmargin 0)
(set! whichwrap "b,s,<,>,h,l,[,],~")
(set! textwidth 80)
;; (set! columns 80)
(set! linebreak)

(set! virtualedit :block)
(set! fileformats [:unix :mac :dos])
(set! clipboard :unnamedplus)
(set! completeopt [:menu :menuone :preview :noinsert])
(set! diffopt+ [:vertical
                :iwhite
                :hiddenoff
                "foldcolumn:0"
                "context:4"
                "algorithm:histogram"
                :indent-heuristic])

(set! foldenable true)
(set! foldcolumn :1)
(set! foldlevel 99)

;; Unfold them all
(set! foldnestmax 0)
(set! foldlevelstart 99)
(set! splitright)
(set! splitbelow)
(set! backspace "indent,eol,start")
(set! switchbuf "useopen,uselast")
(set! eadirection :hor)
(set! sessionoptions "curdir,help,tabpages,winsize")
(set! viewoptions "folds,cursor,curdir,slash,unix")
;; Wildmenu
(set! wildmenu)
(set! wildignorecase)
(set! wildignore+ [:.git
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

(set! wildoptions :pum)
(set! wildmode "longest:full,full")
;; Time
(set! timeout)
(set! ttimeout)
(set! updatetime 50)
(set! timeoutlen 750)

;; give me more time to do key sequences
(set! ttimeoutlen 50)

;; make escape more responsive
(set! redrawtime 1500)
;; Search
(set! ignorecase)
(set! incsearch)
(set! hlsearch)
(set! smartcase)
(set! infercase)
(set! wrapscan)
(set! inccommand :nosplit)
(set! complete ".,w,b,k")
(set! grepformat "%f:%l:%c:%m")
(set! grepprg "rg --hidden --vimgrep --smart-case --")

;;;; VIM DIRECTORIES
(set! undofile)
(set! undolevels 10000)
(set! swapfile false)
(set! backup false)
(set! history 5000)
(set! writebackup false)
(set! directory (.. tdt.paths.STATE-PATH :/swag/))
(set! undodir (.. tdt.paths.STATE-PATH :/undo/))
(set! backupdir (.. tdt.paths.STATE-PATH :/backup/))
(set! viewdir (.. tdt.paths.STATE-PATH :/view/))
(set! spellfile (.. tdt.paths.STATE-PATH :/spell/en.uft-8.add))
(set! backupskip [:/tmp/*
                  :$TMPDIR/*
                  :$TMP/*
                  :$TEMP/*
                  :*/shm/*
                  :/private/var/*
                  :.vault.vim])

;; fnlfmt: skip
(set! shada ["!" "'1000" "<50" "@100" "s10" "h"])
