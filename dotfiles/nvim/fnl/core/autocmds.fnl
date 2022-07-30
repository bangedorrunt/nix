(module core.autocmds
  {require-macros [core.macros]})

; Smart `q` close windows
(autocmd FileType "help,startuptime,qf,lspinfo" "nnoremap <buffer><silent> q :close<CR>")
(autocmd FileType man "nnoremap <buffer><silent> q :quit<CR>")

; Restore cursor on exit
(augroup restore-cursor-on-exit
         (autocmd!)
         (autocmd VimLeave * '(opt guicursor ["a:ver100-blinkon0"])))

; Automatically resize splits when window is resized
(augroup resize-splits-on-resize
         (autocmd!)
         (autocmd VimResized * "wincmd ="))

; Automatically read file when it changes on disk
(augroup read-file-on-disk-change
         (autocmd!)
         (autocmd "FocusGained,BufEnter,CursorHold,CursorHoldI" *
                  '(if (and
                         (not= (nvim.fn.mode) "c")
                         (= (nvim.fn.bufexists "[Command Line]") 0))
                     (vim.api.nvim_command "checktime")))
         (autocmd FileChangedShellPost * "echom 'File changed on disk. Buffer reloaded.'"))

; Open file on last position
(augroup open-file-on-last-position
         (autocmd!)
         (autocmd BufReadPost *
                  '(if (and
                         (> (nvim.fn.line "'\"") 1)
                         (<= (nvim.fn.line "'\"") (nvim.fn.line "$")))
                     (vim.api.nvim_command "normal! g'\""))))

; Disable spell in certain filetypes
(augroup disable-spell-on-filetypes
         (autocmd!)
         (autocmd FileType "help,packer" '(opt-local nospell)))

; Disable colorcolumn in certain filetypes
(augroup disable-colorcolumn-on-filetypes
         (autocmd!)
         (autocmd FileType "help,packer,NvimTree,fern,fennel,clojure,lisp,markdown" '(opt-local colorcolumn [])))

; Remove highlight
(augroup clear-hl-search
         (autocmd!)
         (autocmd CmdlineEnter "/,?" "set hlsearch")
         (autocmd CmdlineLeave "/,?" "set nohlsearch"))

; Set terminal options
(augroup terminal-options
         (autocmd!)
         ; enter terminal-mode (insert) automatically
         (autocmd TermOpen * "startinsert")
         ; disables line number on terminal buffers
         (autocmd TermOpen * '(do
                                (opt-local nonumber)
                                (opt-local norelativenumber)))
         ; disables spell on terminal buffers
         (autocmd TermOpen * '(opt-local nospell))
         ; disables sign column on terminal buffers
         (autocmd TermOpen * '(opt-local signcolumn :no)))
