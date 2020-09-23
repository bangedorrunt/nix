"" ---------------------
"" VIM EASYMOTION CONFIG
"" ---------------------
" Note: can't use non recursive map here
" because <plug>(..) is bound to other
" function call. using non recursive map
" will only bind key to <plug>(..) (which
" in return has no effect) instead of
" binding to function call.

" Tweak the colors
hi link EasyMotionTarget WarningMsg
hi link EasyMotionShade  Comment

" Disable default mapping
let g:EasyMotion_do_mapping=0

" Remap to old version
" map <leader> <plug>(easymotion-prefix)

" Map for 2-character search motion
" nmap <leader>s <plug>(easymotion-s2)
" vmap <leader>s <plug>(easymotion-s2)

" Map for nth-character search motion
" will replace traditional search

nmap / <plug>(easymotion-sn)
omap / <plug>(easymotion-tn)
nmap n <plug>(easymotion-next)
nmap N <plug>(easymotion-prev)

" Line motion
" Note: migrate these keymaps to
" space.rc.vim file

" Make search in line more comfortable
nmap f <plug>(easymotion-f)
nmap t <plug>(easymotion-t)
nmap F <plug>(easymotion-F)
nmap T <plug>(easymotion-T)
