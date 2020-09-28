" NEURON CONFIG
" -----------------

" Neuron dir
let g:neuron_dir = $HOME.'/workspace/notetoself/'
let g:neuron_inline_backlinks = 0
let g:neuron_no_mappings = 0

" Neuron keybinding

nm <leader>zn <plug>EditZettelNew
nm <leader>zN <plug>EditZettelNewFromCword
vm <leader>zN <esc><plug>EditZettelNewFromVisual
nm <leader>zb <plug>NeuronRibStart
nm <leader>zr <plug>NeuronRefreshCache
nm <leader>zu <plug>EditZettelLast
nm <leader>zU :<c-u>call neuron#move_history(-1)<cr>
nm <leader>zP :<c-u>call neuron#move_history(1)<cr>
nm <leader>zz <plug>EditZettelSelect
nm <leader>zZ <plug>EditZettelBacklink
nm <leader>zo <plug>EditZettelUnderCursor
nm <leader>zs <plug>EditZettelSearchContent
nm <leader>zS <plug>EditZettelSearchContentUnderCursor
nm <leader>zl <plug>InsertZettelLast
nm <leader>zi <plug>InsertZettelSelect
nm <leader>zL :<c-u>call neuron#insert_zettel_last(1)<cr>
nm <leader>zI :<c-u>call neuron#insert_zettel_select(1)<cr>
nm <leader>zv <plug>ToggleBacklinks

" VIMWIKI CONFIG
" --------------

" Vimwiki path
" let g:vimwiki_list = [{'path':'$HOME/workspace/note-to-self',
" 											\ 'auto_tags':1, 'auto_toc':1,
" 											\	'syntax':'markdown', 'ext':'.md'}]

" Disable all vimwiki keybindings
" let g:vimwiki_key_mappings =
" \ {
" \   'all_maps': 0,
" \   'global': 0,
" \   'headers': 1,
" \   'text_objs': 1,
" \   'table_format': 1,
" \   'table_mappings': 0,
" \   'lists': 0,
" \   'links': 1,
" \   'html': 1,
" \   'mouse': 0,
" \ }

" let g:vimwiki_map_prefix = '<leader>x'

" VIM-ZETTEL CONFIG
" -----------------
" Text use in backlinks
" let g:zettel_backlinks_title = "Backlinks"
" let g:zettel_link_format="[%title](%link)"
" Use RipGrep
" let g:zettel_fzf_command = "rg --column --line-number --ignore-case --no-heading --color=always "

" let g:zettel_options = [{ "front_matter" : {"tags" : ""},
" 												\ "template" :  "$HOME/workspace/notetoself/zettel.tpl"}]
" let g:zettel_random_chars=8
" let g:zettel_format = "%random"
" let g:zettel_date_format = "%y-%m-%dT%H:%M"

" nnoremap <leader>zn :<c-u>ZettelNew<cr>
" nnoremap <leader>zc :<c-u>ZettelCapture
" nnoremap <leader>zb :<c-u>ZettelBackLinks<cr>
" nnoremap <leader>zz :<c-u>ZettelInbox
" nnoremap <leader>zl :<c-u>ZettelGenerateLinks
" nnoremap <leader>zt :<c-u>ZettelGenerateTags
" nnoremap <leader>zi :<c-u>InsertInsertNote
" nnoremap <leader>zy :<c-u>ZettelYankName<cr>
" nnoremap <leader>zo :<c-u>ZettelOpen
" nnoremap <leader>zs :<c-u>ZettelSearch

