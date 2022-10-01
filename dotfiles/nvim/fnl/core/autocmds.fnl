(import-macros {: lazyreq
                : augroup
                : autocmd
                : autocmd!
                : set!
                : setl!
                : nmap
                : noremap} :core.macros)

;; Packer autocommands
(augroup packer-compile
  (autocmd!)
  (autocmd BufWritePost */fnl/mod/init.fnl `(vim.cmd "PackerCompile profile=true"))
  (autocmd VimLeavePre */fnl/mod/init.fnl `(vim.cmd "PackerCompile profile=true")))

;; Smart `q` close windows
(augroup smart-quit
  (autocmd!)
  (autocmd FileType [help startuptime qf lspinfo]
           `(noremap n buffer :q :<Cmd>close<CR>))
  (autocmd FileType man `(noremap n buffer :q :<Cmd>quit<CR>))
  (autocmd FileType [fugitive fugitiveblame]
           `(nmap n buffer :q :gq)))

;; Toggle relative number
(augroup toggle-relative-number
  (autocmd!)
  (autocmd [BufLeave FocusLost WinLeave InsertEnter] * `(set! norelativenumber))
  (autocmd [BufEnter FocusGained WinEnter InsertLeave] * `(set! relativenumber)))

;; Toggle OpenKey
;; NOTE: macOS only
(augroup toggle-evkey
  (autocmd!)
  (autocmd InsertEnter [*.md *.markdown *.norg]
           `(os.execute "osascript -e 'tell application \"System Events\" to keystroke \"z\" using option down'"))
  (autocmd InsertLeave [*.md *.markdown *.norg]
           `(os.execute "osascript -e 'tell application \"System Events\" to keystroke \"z\" using option down'")))

;; Restore cursor on exit
(augroup restore-cursor-on-exit
  (autocmd!)
  (autocmd VimLeave * `(set! guicursor ["a:ver100-blinkon0"])))

;; Show cursor line only in active window
(augroup cursor-line-in-active-window
  (autocmd!)
  (autocmd [InsertLeave WinEnter] * `(set! cursorline))
  (autocmd [InsertEnter WinLeave] * `(set! nocursorline)))

;; Enable "onemore" in visual mode
(augroup enable-onemore-in-visual-mode
  ;; `<` is so arcane, nobody thought about it when adding Lua options
  ;; Workaround `vim.opt_global.foo = vim.setl.foo:get()`
  (autocmd!)
  (autocmd ModeChanged "*:[v]*" `(vim.cmd.setl :virtualedit+=onemore))
  (autocmd ModeChanged "[v]*:*" `(vim.cmd.setl :virtualedit<)))

;; Automatically resize splits when window is resized
(augroup resize-splits-on-resize
  (autocmd!)
  (autocmd VimResized * "wincmd ="))

;; Automatically read file when it changes on disk
(augroup read-file-on-disk-change
  (autocmd!)
  (autocmd [FocusGained BufEnter CursorHold CursorHoldI] *
           `(if (and (not= (vim.fn.mode) :c)
                     (= (vim.fn.bufexists "[Command Line]") 0))
                (vim.cmd.checktime)))
  (autocmd FileChangedShellPost *
           "echom 'File changed on disk. Buffer reloaded.'"))

;; Open file on last position
(augroup open-file-on-last-position
  (autocmd!)
  (autocmd BufReadPost *
           `(if (and (> (vim.fn.line "'\"") 1)
                     (<= (vim.fn.line "'\"") (vim.fn.line "$")))
                (vim.cmd.normal {:args ["g'\""] :bang true}))))

;; Disable spell in certain filetypes
(augroup disable-spell-on-filetypes
  (autocmd!)
  (autocmd FileType [help packer] `(setl! nospell)))

;; Disable colorcolumn in certain filetypes
(augroup disable-colorcolumn-on-filetypes
  (autocmd!)
  (autocmd FileType [help packer NvimTree fern
                     fennel clojure lisp
                     markdown]
           `(setl! colorcolumn [])))

;; Remove highlight
(augroup clear-hl-search (autocmd!)
  (autocmd CmdlineEnter [/ ?] `(set! hlsearch))
  (autocmd CmdlineLeave [/ ?] `(set! nohlsearch)))

;; Set terminal options
(augroup terminal-options
  (autocmd!)
  ;; enter terminal-mode (insert) automatically
  (autocmd TermOpen * :startinsert)
  ;; disables line number on terminal buffers
  (autocmd TermOpen *
           `(do
              (setl! nonumber)
              (setl! norelativenumber)))
  ;; disables spell on terminal buffers
  (autocmd TermOpen * `(setl! nospell))
  ;; disables sign column on terminal buffers
  (autocmd TermOpen * `(setl! signcolumn :no)))
