" PRETTIER CONFIG
" ---------------

" let g:prettier#exec_cmd_path = '/usr/local/bin/prettier'
let g:prettier#config#config_precedence = 'file-override'
let g:prettier#autoformat_require_pragma = 0

" Change default Prettier keybinding
nmap <Leader>cf <Plug>(Prettier)
