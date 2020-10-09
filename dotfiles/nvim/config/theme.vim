" -----------
" THEME
" -----------

" Enable true color
if has('termguicolors')
  set termguicolors
endif

" Sonokai
" -------

" let g:sonokai_style = 'atlantis'
" let g:sonokai_enable_italic             = 1
" let g:sonokai_diagnostic_line_highlight = 1
" let g:sonokai_better_performance        = 1
" colorscheme sonokai

" Iceberg
" -------

set background=light
colorscheme iceberg


let g:lightline = {
      \ 'colorscheme': 'iceberg',
      \ }

" I hate error background color
" hi Error guibg=NONE ctermbg=NONE
" hi ErrorMsg guibg=NONE ctermbg=NONE
" hi NvimInternalError guibg=NONE ctermbg=NONE
