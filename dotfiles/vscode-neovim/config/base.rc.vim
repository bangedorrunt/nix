" neovim initialization
" ------------------
" neovim conforms to the xdg base directory specification
" $XDG_CONFIG_HOME: ~/.config/nvim
" $XDG_DATA_HOME: ~/.local/share/nvim

" neovim automatically create `shada`, `swap`, `undo` dirs under $XDG_DATA_HOME

let $NVIM_PATH = expand($HOME.'/.config/nvim')
let $CACHE_PATH = expand(($XDG_CACHE_HOME ? $XDG_CACHE_HOME : '~/.cache').'/vim')

" ensure custom spelling directory
if !isdirectory(expand('$NVIM_PATH/spell'))
  call mkdir(expand('$NVIM_PATH/spell'))
endif

" load vault settings
if filereadable(expand('$NVIM_PATH/.vault.vim'))
  execute 'source' expand('$NVIM_PATH/.vault.vim')
endif

" python interpreter settings
" python2 is end of life cycle, use python3 only
" use pyenv to manage python, let neovim automatically find its path
" let g:python3_host_prog = '/usr/local/bin/python3'

" disable menu.vim
if has('gui_running')
  set guioptions=Mc
endif

" disable providers we do not give a shit about
let g:loaded_python_provider = 0
let g:loaded_ruby_provider = 0
let g:loaded_perl_provider = 0
" let g:loaded_node_provider = 0

" disable some in built plugins completely
let g:loaded_netrw            = 1
let g:loaded_netrwPlugin      = 1
let g:loaded_matchparen       = 1
let g:loaded_matchit          = 1
let g:loaded_2html_plugin     = 1
let g:loaded_getscriptPlugin  = 1
let g:loaded_gzip             = 1
let g:loaded_logipat          = 1
let g:loaded_rrhelper         = 1
let g:loaded_spellfile_plugin = 1
let g:loaded_tarPlugin        = 1
let g:loaded_vimballPlugin    = 1
let g:loaded_zipPlugin        = 1
