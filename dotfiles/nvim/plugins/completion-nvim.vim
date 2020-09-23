" COMPLETION-NVIM CONFIG
" ----------------------

let g:diagnostic_insert_delay        = 1
let g:diagnostic_show_sign           = 1
let g:diagnostic_enable_virtual_text = 1
let g:completion_enable_auto_paren   = 1

" Use <c-space> to manually trigger completion
inoremap <silent><expr> <c-space> completion#trigger_completion()

" Setup chain completion
let g:completion_chain_complete_list = [
    \{'complete_items': ['lsp', 'snippet', 'buffers']}
\]

" Use completion-nvim in every buffer
au TYPESRC BufEnter * lua require'completion'.on_attach()
