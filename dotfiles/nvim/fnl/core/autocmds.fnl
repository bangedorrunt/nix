(import-macros {: augroup : autocmd : autocmd!
                : opt : opt_local
                : nmap : noremap} :core.macros)

;; Packer autocommands
(autocmd BufWritePost */plugins/init.fnl '(vim.cmd "PackerCompile profile=true"))
(autocmd VimLeavePre [*/plugins/init.fnl */plugins/**/*.fnl] '(vim.cmd "PackerCompile profile=true"))

;; Open help vertically
(autocmd FileType [help startuptime lspinfo man] '(vim.cmd.wincmd "L"))

;; Smart `q` close windows
(augroup smart_q
         (autocmd!)
         (autocmd FileType [help startuptime qf lspinfo] '(noremap n buffer silent :q "<Cmd>close<CR>"))
         (autocmd FileType man '(noremap n buffer silent :q "<Cmd>quit<CR>"))
         ;; BUG: q don't work with fugitive
         (autocmd FileType [fugitive fugitiveblame] '(nmap n buffer :q :gq)))

;; Restore cursor on exit
(augroup restore_cursor_on_exit
         (autocmd!)
         (autocmd VimLeave * '(opt guicursor ["a:ver100-blinkon0"])))

;; Automatically resize splits when window is resized
(augroup resize_splits_on_resize
         (autocmd!)
         (autocmd VimResized * "wincmd ="))

;; Automatically read file when it changes on disk
(augroup read_file_on_disk_change
         (autocmd!)
         (autocmd [FocusGained BufEnter CursorHold CursorHoldI] *
                  '(if (and
                         (not= (vim.fn.mode) "c")
                         (= (vim.fn.bufexists "[Command Line]") 0))
                     (vim.cmd.checktime)))
         (autocmd FileChangedShellPost * "echom 'File changed on disk. Buffer reloaded.'"))

;; Open file on last position
(augroup open_file_on_last_position
         (autocmd!)
         (autocmd BufReadPost *
                  '(if (and
                         (> (vim.fn.line "'\"") 1)
                         (<= (vim.fn.line "'\"") (vim.fn.line "$")))
                     (vim.cmd.normal {:args ["g'\""] :bang true}))))

;; Disable spell in certain filetypes
(augroup disable_spell_on_filetypes
         (autocmd!)
         (autocmd FileType [help packer] '(opt_local nospell)))

;; Disable colorcolumn in certain filetypes
(augroup disable_colorcolumn_on_filetypes
         (autocmd!)
         (autocmd FileType [help packer NvimTree fern fennel clojure lisp markdown] '(opt_local colorcolumn [])))

;; Remove highlight
(augroup clear_hl_search
         (autocmd!)
         (autocmd CmdlineEnter [/ ?] '(opt hlsearch))
         (autocmd CmdlineLeave [/ ?] '(opt nohlsearch)))

;; Set terminal options
(augroup terminal_options
         (autocmd!)
         ;; enter terminal-mode (insert) automatically
         (autocmd TermOpen * "startinsert")
         ;; disables line number on terminal buffers
         (autocmd TermOpen * '(do
                                (opt_local nonumber)
                                (opt_local norelativenumber)))
         ;; disables spell on terminal buffers
         (autocmd TermOpen * '(opt_local nospell))
         ;; disables sign column on terminal buffers
         (autocmd TermOpen * '(opt_local signcolumn :no)))
