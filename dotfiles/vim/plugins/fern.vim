" FERN.VIM CONFIG
" ---------------

" let g:fern#default_hidden = 1
" let g:fern#default_exclude = &wildignore

" Toggle side bar and auto ballance windows
nnoremap <silent> <leader>ts :<c-u>Fern . -toggle -drawer -stay -reveal=%<cr><c-w>=

function! s:enable_clean_ui() abort
  setlocal listchars=
    \ nonumber
    \ norelativenumber
    \ nowrap
    \ winfixwidth
    \ laststatus=0
    \ noshowmode
    \ noruler
    \ scl=no
    \ colorcolumn=
  autocmd BufLeave <buffer> set laststatus=2 showmode ruler
endfunction


" function! s:fern_preview_init() abort
"   nmap <buffer><expr>
"         \ <Plug>(fern-my-preview-or-nop)
"         \ fern#smart#leaf(
"         \   "\<Plug>(fern-action-open:edit)\<C-w>p",
"         \   "",
"         \ )
"   nmap <buffer><expr> j
"         \ fern#smart#drawer(
"         \   "j\<Plug>(fern-my-preview-or-nop)",
"         \   "j",
"         \ )
"   nmap <buffer><expr> k
"         \ fern#smart#drawer(
"         \   "k\<Plug>(fern-my-preview-or-nop)",
"         \   "k",
"         \ )
" endfunction

augroup FERN
  au! *
  au FileType fern call s:enable_clean_ui()
augroup END
