" NEOVIM INITIALIZATION
" ------------------
" Neovim conforms to the xdg base directory specification
" $XDG_CONFIG_HOME: ~/.config/nvim
" $XDG_DATA_HOME: ~/.local/share/nvim

" Neovim automatically create `shada`, `swap`, `undo` dirs under $XDG_DATA_HOME

let $NVIM_PATH = expand($HOME.'/.config/nvim')
let $CACHE_PATH = expand(($XDG_CACHE_HOME ? $XDG_CACHE_HOME : '~/.cache').'/nvim')

" Ensure data directories
for s:path in [
      \ $CACHE_PATH,
      \ $CACHE_PATH.'/undo',
      \ $CACHE_PATH.'/backup',
      \ $CACHE_PATH.'/session',
      \ $NVIM_PATH .'/spell' ]
  if ! isdirectory(s:path)
    call mkdir(s:path, 'p')
  endif
endfor

if &runtimepath !~# $NVIM_PATH
  set runtimepath^=$NVIM_PATH
  set runtimepath+=$NVIM_PATH/after
endif

" Load vault settings
if filereadable(expand('$NVIM_PATH/.vault.vim'))
  execute 'source' expand('$NVIM_PATH/.vault.vim')
endif

" Disable menu.vim
if has('gui_running')
  set guioptions=Mc
endif

" Disable providers we do not give a shit about
let g:loaded_python_provider   = 0
let g:loaded_ruby_provider     = 0
let g:loaded_perl_provider     = 0

" Disable some in built plugins completely
let g:loaded_matchit           = 1
let g:loaded_matchparen        = 1
let g:loaded_2html_plugin      = 1
let g:loaded_logiPat           = 1
let g:loaded_rrhelper          = 1

let g:loaded_getscript         = 1
let g:loaded_getscriptPlugin   = 1
let g:loaded_vimball           = 1
let g:loaded_vimballPlugin     = 1

let g:loaded_netrw             = 1
let g:loaded_netrwPlugin       = 1
let g:loaded_netrwSettings     = 1
let g:loaded_netrwFileHandlers = 1
