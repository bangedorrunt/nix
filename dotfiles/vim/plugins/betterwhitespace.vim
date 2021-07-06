"" ----------------------------
"" VIM-BETTER-WHITESPACE CONFIG
"" ----------------------------
let g:better_whitespace_ctermcolor='NONE'
let g:better_whitespace_guicolor='NONE'
" Turn off white space highlight
au TYPESRC BufWritePre * StripWhitespace

" Automatically strip white space on save
au TYPESRC BufWrite * ToggleStripWhitespaceOnSave
