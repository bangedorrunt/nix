"" -------------------
"" VIM-STARTIFY CONFIG
"" -------------------
nmap <silent> <f1> :<c-u>Startify<cr>

au TYPESRC FileType startify setlocal cursorline

let g:startify_relative_path          = 0
let g:startify_change_to_dir          = 0
let g:startify_session_autoload       = 1
let g:startify_session_persistence    = 1
let g:startify_session_delete_buffers = 0

let g:startify_session_dir = '$CACHE_PATH/session'

let g:startify_bookmarks = [
  \ {'i': '$HOME/.cache/nvim/plugged/iceberg.vim/src/iceberg.vim'},
  \ {'m': '$HOME/.tmux.conf'},
  \ {'v': '$NVIM_PATH/init.vim'},
  \ {'z': '$HOME/dotfiles/.zshrc'}
  \ ]

let g:startify_list_order = [
  \ ['   Sessions:'],
  \ 'sessions',
  \ ['   LRU:'],
  \ 'files',
  \ ['   LRU within this dir:'],
  \ 'dir',
  \ ['   Bookmarks:'],
  \ 'bookmarks',
  \ ]

let g:startify_skiplist = [
            \ 'COMMIT_EDITMSG',
            \ $VIMRUNTIME .'/doc',
            \ 'bundle/.*/doc',
            \ '\.vimgolf',
            \ ]
