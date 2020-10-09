"" ---------------------
"" ESSENTIAL KEYBINDINGS
"" ---------------------

" Quick escape mode
imap fd <esc>
tnoremap fd <c-\><c-n>

" Force quit all
nmap <silent> <c-q> :<c-u>confirm qa<cr>

" Use <tab> and <s-tab> to jump to buffers
nnoremap <tab> :<c-u>bn<cr>
nnoremap <s-tab> :<c-u>bp<cr>

" Break line in normal mode
nmap o o<esc>
nmap O O<esc>

" Use `emacs` keybinding in insert mode
inoremap <c-w> <c-[>diwa
inoremap <c-h> <bS>
inoremap <c-d> <del>
inoremap <c-u> <c-g>u<c-u>
inoremap <c-b> <left>
inoremap <c-f> <right>
inoremap <c-a> <home>
inoremap <expr><c-e> pumvisible() ? "\<c-e>" : "\<end>"

" Smart way to move between windows
nmap <c-j> <c-w>j
nmap <c-k> <c-w>k
nmap <c-h> <c-w>h
nmap <c-l> <c-w>l
nmap <c-c> <c-w>c

" Use arrow keys to resize window panes
nnoremap <left> :vertical resize +1<cr>
nnoremap <right> :vertical resize -1<cr>
nnoremap <up> :resize -1<CR>
nnoremap <down> :resize +1<CR>

" Make j and k move to next line, not a file line
" nnoremap j gj
" nnoremap k gk

nmap j <plug>(accelerated_jk_gj)
nmap k <plug>(accelerated_jk_gk)

" Prevent x from overriding what's in the clipboard
nnoremap x "_x
nnoremap X "_x

" Copy/Paste/Cut
nmap YY "+y<cr>
nmap P "+gP<cr>
nmap XX "+x<cr>

" Clean search (highlight)
nmap <silent> <f2> :<c-u>noh<cr>

" Prevent selecting and pasting from overwritting what you originally copied
xnoremap p pgvy

" Keep cursor at the bottom of the visual selection after you yank it
vmap y ygv<esc>

" Maintain visual mode after shifting > and <
vmap < <gv
vmap > >gv

nmap >> >>_
nmap << <<_

" Saving read-only file
cmap w!! w !sudo tee % >/dev/null

" Use <tab> and <s-tab> for navigate completion list
inoremap <expr> <tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <expr> <s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"

" Use <cr> to confirm completion
inoremap <expr> <cr> pumvisible() ? "\<c-y>" : "\<c-g>u\<cr>"

" Quit quickfix window by <esc>"
augroup TYPESRC
	au FileType qf nnoremap <buffer><silent> <esc> :quit<cr>

	" quit `fzf` window by <esc>
	" au FileType fzf <buffer> nnoremap <silent> <esc> <c-c>
augroup END

" Identify the syntax highlighting group under the cursor
map <f10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
  \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
  \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
