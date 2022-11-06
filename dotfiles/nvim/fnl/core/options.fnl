(import-macros {: set!} :core.macros)

(fn setup []
  ;;;; RENDERING
  ;; (set! background :light)

  ;;;; UI
  ;; (set! lz)
  (set! number)
  (set! relativenumber)
  (set! termguicolors)
  ;; (set! spell)
  (set! cursorline)
  (set! report 0)
  (set! novisualbell)
  (set! noerrorbells)
  (set! mouse :a)
  (set! showmatch)
  (set! matchtime 2)
  (set! shortmess :FOotnficTWlx)
  (set! pumheight 0)
  (set! winwidth 30)
  (set! winminwidth 10)
  (set! winminheight 0)
  (set! helpheight 12)
  (set! previewheight 12)
  (set! cmdwinheight 12)
  (set! cmdheight 0)
  (set! conceallevel 2)
  (set! concealcursor :nc)
  (set! signcolumn :yes)
  ;; (set! colorcolumn :80)
  (set! noshowmode)
  (set! laststatus 3)

  (set! expandtab)

  ;; use space instead of tab
  (set! tabstop 2)
  (set! shiftround)
  (set! shiftwidth 2)

  ;; in favour of tabstop
  (set! softtabstop -1)
  (set! smartindent)
  (set! breakindent)

  ;; maintain indentation on break
  (set! showbreak "↳ ")
  (set! breakindentopt "sbr")
  ;; Invisibles
  (set! list)

  ;; show invisible chars
  (set! listchars {:eol nil
                   :tab "  "
                   :nbsp "+"
                   :trail "·"
                   :extends "›"
                   :precedes "‹"})

  (set! fillchars {:vert "│"
                   :eob " "
                   :diff " "
                   :fold " "
                   :foldopen ""
                   :foldsep ""
                   :foldclose ""})

  ;;;; BEHAVIOUR
  (set! splitright)
  (set! splitbelow)
  (set! splitkeep :screen)
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
  (set! completeopt [:menu :menuone :noselect])
  (set! diffopt+ [:vertical
                  :iwhite
                  :hiddenoff
                  "foldcolumn:0"
                  "context:4"
                  "algorithm:histogram"
                  :indent-heuristic
                  "linematch:60"])

  (set! foldenable)
  (set! foldcolumn :1)
  (set! foldlevel 99)
  (set! foldnestmax 0)
  (set! foldlevelstart 99)
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
  (set! grepprg "rg --hidden --vimgrep --smart-case --no-heading --follow $*")

  ;;;; VIM DIRECTORIES
  (set! undofile)
  (set! undolevels 10000)
  (set! noswapfile)
  (set! nobackup)
  (set! history 5000)
  (set! nowritebackup)
  (set! directory (.. store.paths.state :/swag/))
  (set! undodir (.. store.paths.state :/undo/))
  (set! backupdir (.. store.paths.state :/backup/))
  (set! viewdir (.. store.paths.state :/view/))
  (set! spellfile (.. store.paths.state :/spell/en.uft-8.add))
  (set! backupskip [:/tmp/*
                    :$TMPDIR/*
                    :$TMP/*
                    :$TEMP/*
                    :*/shm/*
                    :/private/var/*
                    :.vault.vim])

  ;; fnlfmt: skip
  (set! shada ["!" "'1000" "<50" "@100" "s10" "h"]))

{: setup}
