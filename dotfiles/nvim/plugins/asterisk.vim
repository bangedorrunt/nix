" -------------------
" VIM ASTERISK CONFIG
" -------------------

" Also keep search result at center of screen
nnoremap n nzz
nnoremap N Nzz

" map *   <plug>(asterisk-*)
" map g*  <plug>(asterisk-g*)
" map #   <plug>(asterisk-#)
" map g#  <plug>(asterisk-g#)

" map z*  <plug>(asterisk-z*)
" map gz* <plug>(asterisk-gz*)
" map z#  <plug>(asterisk-z#)
" map gz# <plug>(asterisk-gz#)

map *  <plug>(asterisk-z*)<plug>(is-nohl-1)
map g* <plug>(asterisk-gz*)<plug>(is-nohl-1)
map #  <plug>(asterisk-z#)<plug>(is-nohl-1)
map g# <plug>(asterisk-gz#)<plug>(is-nohl-1)

" Enable keepcursor
let g:asterisk#keeppos = 1

