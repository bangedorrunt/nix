(module core.options
  {autoload {nvim aniseed.nvim}
   require-macros [core.macros]})

;;;; RENDERING
;; (o background :light)

(if (has? :termguicolors)
  (do
    (vim.cmd "let &t_8f = '\\<Esc>[38;2;%lu;%lu;%lum'")
    (vim.cmd "let &t_8b = '\\<Esc>[48;2;%lu;%lu;%lum'")
    (o termguicolors))
  nil)

;;;; UI
(o lz)
(o number)
(o report 0)
(o visualbell false)
(o errorbells false)
(o mouse "a")
(o cursorline)
(o showmatch)
(o matchtime 2)
(o shortmess "cA")
(o pumblend 0)
(o pumheight 15)
(o winblend 0)
(o winwidth 30)
(o winminwidth 10)
(o winminheight 0)
(o helpheight 12)
(o previewheight 12)
(o cmdwinheight 12)
(o conceallevel 2)
(o concealcursor "niv")
(o signcolumn "yes")
;; Statusline
(o showmode false)
(o laststatus 2)
;; Spacing
(o textwidth 80)
(o expandtab)
(o tabstop 2)
(o shiftwidth 2)
(o softtabstop -1)
(o smarttab)
(o autoindent)
(o smartindent)
(o shiftround)
(o showbreak "↳ ")
(o breakindentopt "shift:2,min:20")
;; Invisibles
(o list)
(o listchars {:tab "»·"
              :nbsp       "+"
              :trail      "·"
              :extends    "→"
              :precedes   "←"})
(o fillchars {:vert "▕"
              :fold      "·"
              :diff      ""
              :msgsep    "‾"
              :eob       " "
              :foldopen  "▾"
              :foldsep   "│"
              :foldclose "▸"})

;;;; BEHAVIOUR
;; fnlfmt: skip
(o magic)
;; (o hidden)
(o autoread)
(o wrap)
(o whichwrap "b,s,<,>,h,l,[,],~")
(o nolinebreak)
(o virtualedit "block")
(o fileformats "unix,mac,dos")
(o clipboard "unnamedplus") 
(o completeopt "menuone,noselect")
(o diffopt+  ["vertical" "iwhite" "hiddenoff" "foldcolumn:0" "context:4" "algorithm:histogram" "indent-heuristic"])
(o splitright)
(o splitbelow)
(o backspace "indent,eol,start")
(o switchbuf "useopen,uselast")
(o eadirection "hor")
(o sessionoptions "curdir,help,tabpages,winsize")
(o viewoptions "folds,cursor,curdir,slash,unix")
;; Wildmenu
(o wildmenu)
(o wildignorecase)
(o wildignore+ [".git"
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
(o wildoptions "pum")
(o wildmode "longest:full,full")
;; Time
(o timeout)
(o ttimeout)
(o updatetime 100)
(o timeoutlen 350)
(o ttimeoutlen 10)
(o redrawtime 1500)
;; Search
(o ignorecase)
(o incsearch)
(o hlsearch)
(o smartcase)
(o infercase)
(o wrapscan)
(o inccommand "nosplit")
(o complete ".,w,b,k")
(o grepformat "%f:%l:%c:%m")
(o grepprg "rg --hidden --vimgrep --smart-case --")

;;;; VIM DIRECTORIES
(o undofile)
(o swapfile false)
(o backup false)
(o history 5000)
(o writebackup false)
(o directory (.. tdt.paths.CACHE_PATH "/swag/"))
(o undodir (.. tdt.paths.CACHE_PATH "/undo/"))
(o backupdir (.. tdt.paths.CACHE_PATH "/backup/"))
(o viewdir (.. tdt.paths.CACHE_PATH "/view/"))
(o spellfile (.. tdt.paths.CACHE_PATH "/spell/en.uft-8.add"))
(o backupskip ["/tmp/*"
               "$TMPDIR/*"
               "$TMP/*"
               "$TEMP/*"
               "*/shm/*"
               "/private/var/*"
               ".vault.vim"])
(o shada ["!" "'1000" "<50" "@100" "s10" "h"])

