(module core.options {require-macros [core.macros]})

; COLORS
(set! syntax "enable")

; RENDERING
(set! encoding "utf-8")
(set! synmaxcol 1777)

(if (vim.fn.has "termguicolors")
  (do
    (vim.cmd "let &t_8f = '\\<Esc>[38;2;%lu;%lu;%lum'")
    (vim.cmd "let &t_8b = '\\<Esc>[48;2;%lu;%lu;%lum'")
    (set! termguicolors))
  nil)

; UI
(set! lz)
(set! number)
(set! report 0)
(set! visualbell false)
(set! errorbells false)
(set! mouse "a")
(set! cursorline)
(set! showmatch)
(set! matchtime 2)
(set! shortmess "IcT")
(set! pumblend 0)
(set! pumheight 15)
(set! winblend 0)
(set! winwidth 30)
(set! winminwidth 10)
(set! winminheight 0)
(set! helpheight 12)
(set! previewheight 12)
(set! cmdwinheight 12)
(set! conceallevel 2)
(set! concealcursor "niv")
(set! signcolumn "yes")

; BEHAVIOUR
(set! hidden)
(set! magic)
(set-local! so 10)
(set! autoread)
(set! wrap)
(set! whichwrap "b,s,<,>,h,l,[,],~")
(set! nolinebreak)
(set! virtualedit "block")
(set! fileformats "unix,mac,dos")
(set! clipboard "unnamedplus") ; don't forget xsel!
(set! completeopt "menuone,noselect")
(set! diffopt+ ["vertical" "iwhite" "hiddenoff" "foldcolumn:0" "context:4" "algorithm:histogram" "indent-heuristic"])
(set! splitright)
(set! splitbelow)
(set! backspace ["indent" "eol" "start"])
(set! switchbuf ["useopen" "uselast"])
(set! eadirection "hor")
(set! sessionoptions "curdir,help,tabpages,winsize")
(set! viewoptions "folds,cursor,curdir,slash,unix")

;; Wildmenu
(set! wildmenu)
(set! wildignorecase)
(set! wildignore+ [".git"
                   ".hg"
                   ".svn"
                   "*.o"
                   "*.out"
                   "*.jpg"
                   "*.jpeg"
                   "*.png"
                   "*.gif"
                   "*.zip"
                   "*~"
                   "**/tmp/** *.DS_Store"
                   "**/node_modules/**"
                   "**/bower_modules/**"
                   "*.pyc"
                   "*pycache*"])
(set! wildoptions "pum")
(set! wildmode "longest:full,full")

; VIM DIRECTORIES
(set! undofile)
(set! swapfile false)
(set! backup false)
(set! history 5000)
(set! writebackup false)
(set! directory (.. tdt.paths.CACHE_DIR "/swag/"))
(set! undodir (.. tdt.paths.CACHE_DIR "/undo/"))
(set! backupdir (.. tdt.paths.CACHE_DIR "/backup/"))
(set! viewdir (.. tdt.paths.CACHE_DIR "/view/"))
(set! spellfile (.. tdt.paths.CACHE_DIR "/spell/en.uft-8.add"))
(set! backupskip ["/tmp/*"
                  "$TMPDIR/*"
                  "$TMP/*"
                  "$TEMP/*"
                  "*/shm/*"
                  "/private/var/*"
                  ".vault.vim"])
(set! shada ["!" "'1000" "<50" "@100" "s10" "h"])

; STATUS LINES
(set! showmode false)
(set! laststatus 2)

; TIME
(set! timeout)
(set! ttimeout)
(set! updatetime 100)
(set! timeoutlen 350)
(set! ttimeoutlen 10)
(set! redrawtime 1500)

; SEARCH
(set! ignorecase)
(set! incsearch)
(set! hlsearch)
(set! smartcase)
(set! infercase)
(set! wrapscan)
(set! inccommand "nosplit")
(set! complete ".,w,b,k")
(set! grepformat "%f:%l:%c:%m")
(set! grepprg "rg --hidden --vimgrep --smart-case --")

; FOLDNG
(set! fen)
(set! foldlevelstart 99) ;; Start with everything unfold
(set! foldtext #(vim.fn.printf "  %-6d%s"
                               (- vim.v.foldend (+ vim.v.foldstart 1))
                               (vim.fn.getline vim.v.foldstart)))

; SPACING
(set! textwidth 80)
(set! expandtab)
(set! tabstop 2)
(set! shiftwidth 2)
(set! softtabstop -1)
(set! smarttab)
(set! autoindent)
(set! smartindent)
(set! shiftround)
(set! showbreak "↳ ")
(set! breakindentopt "shift:2,min:20")

; INVISIBLES
(set! listchars {:tab "»·"
                 :nbsp "+"
                 :trail "·"
                 :extends "→"
                 :precedes "←"})
(set! :list)
(set! fillchars  {:vert "▕"
                  :fold "·"
                  :diff ""
                  :msgsep "‾"
                  :eob " "
                  :foldopen "▾"
                  :foldsep "│"
                  :foldclose "▸"})

