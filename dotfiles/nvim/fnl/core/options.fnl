(module core.options
  {require-macros [core.macros]})

;;;; COLORS
(set! syntax "enable")

;;;; RENDERING
(set! encoding "utf-8"
      synmaxcol 1777)

(if (vim.fn.has "termguicolors")
  (do
    (vim.cmd "let &t_8f = '\\<Esc>[38;2;%lu;%lu;%lum'")
    (vim.cmd "let &t_8b = '\\<Esc>[48;2;%lu;%lu;%lum'")
    (set! termguicolors))
  nil)

;;;; UI
(set! lz
      number
      report 0
      visualbell false
      errorbells false
      mouse "a"
      cursorline
      showmatch
      matchtime 2
      shortmess "IcT"
      pumblend 0
      pumheight 15
      winblend 0
      winwidth 30
      winminwidth 10
      winminheight 0
      helpheight 12
      previewheight 12
      cmdwinheight 12
      conceallevel 2
      concealcursor "niv"
      signcolumn "yes"
      ;; Statusline
      showmode false
      laststatus 2
      ;; Spacing
      textwidth 80
      expandtab
      tabstop 2
      shiftwidth 2
      softtabstop -1
      smarttab
      autoindent
      smartindent
      shiftround
      showbreak "↳ "
      breakindentopt "shift:2,min:20"
      ;; Invisibles
      list
      listchars {:tab "»·"
                 :nbsp "+"
                 :trail "·"
                 :extends "→"
                 :precedes "←"}
      fillchars  {:vert "▕"
                  :fold "·"
                  :diff ""
                  :msgsep "‾"
                  :eob " "
                  :foldopen "▾"
                  :foldsep "│"
                  :foldclose "▸"})

;;;; BEHAVIOUR
(set-local! so 10)
(set! hidden
      magic
      autoread
      wrap
      whichwrap "b,s,<,>,h,l,[,],~"
      nolinebreak
      virtualedit "block"
      fileformats "unix,mac,dos"
      clipboard "unnamedplus" ; don't forget xsel!
      completeopt "menuone,noselect"
      diffopt+ ["vertical" "iwhite" "hiddenoff" "foldcolumn:0" "context:4" "algorithm:histogram" "indent-heuristic"]
      splitright
      splitbelow
      backspace ["indent" "eol" "start"]
      switchbuf ["useopen" "uselast"]
      eadirection "hor"
      sessionoptions "curdir,help,tabpages,winsize"
      viewoptions "folds,cursor,curdir,slash,unix"
      ;; Wildmenu
      wildmenu
      wildignorecase
      wildignore+ [".git"
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
                   "*pycache*"]
      wildoptions "pum"
      wildmode "longest:full,full"
      ;; Time
      timeout
      ttimeout
      updatetime 100
      timeoutlen 350
      ttimeoutlen 10
      redrawtime 1500
      ;; Search
      ignorecase
      incsearch
      hlsearch
      smartcase
      infercase
      wrapscan
      inccommand "nosplit"
      complete ".,w,b,k"
      grepformat "%f:%l:%c:%m"
      grepprg "rg --hidden --vimgrep --smart-case --"
      ;; Foldng
      fen false
      foldlevelstart 99 ;; Start with everything unfold
      foldtext #(vim.fn.printf "  %-6d%s"
                               (- vim.v.foldend (+ vim.v.foldstart 1))
                               (vim.fn.getline vim.v.foldstart)))

;;;; VIM DIRECTORIES
(set! undofile
      swapfile false
      backup false
      history 5000
      writebackup false
      directory (.. tdt.paths.CACHE_DIR "/swag/")
      undodir (.. tdt.paths.CACHE_DIR "/undo/")
      backupdir (.. tdt.paths.CACHE_DIR "/backup/")
      viewdir (.. tdt.paths.CACHE_DIR "/view/")
      spellfile (.. tdt.paths.CACHE_DIR "/spell/en.uft-8.add")
      backupskip ["/tmp/*"
                  "$TMPDIR/*"
                  "$TMP/*"
                  "$TEMP/*"
                  "*/shm/*"
                  "/private/var/*"
                  ".vault.vim"]
      shada ["!" "'1000" "<50" "@100" "s10" "h"])




