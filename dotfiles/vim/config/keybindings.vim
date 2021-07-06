"" ---------------------
"" ESSENTIAL KEYBINDINGS
"" ---------------------

" Quick Escape mode
" imap fd <Esc>
" tnoremap fd <C-\><C-n>

" I don't use macro, make `q` useful
nmap <silent> q <C-c>
" Force quit all
nmap <silent> <C-q> :<C-u>confirm qa<CR>

" Use <Tab> and <S-Tab> to jump to buffers
nnoremap <Tab> :<C-u>bn<CR>
nnoremap <S-Tab> :<C-u>bp<CR>

" Break line in normal mode
nmap o o<Esc>
nmap O O<Esc>

" Use `emacs` keybinding in insert mode
" inoremap <C-w> <C-[>diwa
" inoremap <C-h> <bS>
" inoremap <C-d> <del>
" inoremap <C-u> <C-g>u<C-u>
" inoremap <C-b> <left>
" inoremap <C-f> <right>
" inoremap <C-a> <home>
" inoremap <expr><C-e> pumvisible() ? "\<C-e>" : "\<end>"

" Smart way to move between windows
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-h> <C-w>h
nmap <C-l> <C-w>l
nmap <C-c> <C-w>c

" Use arrow keys to resize window panes
nnoremap <left> :vertical resize +1<CR>
nnoremap <right> :vertical resize -1<CR>
nnoremap <up> :resize -1<CR>
nnoremap <down> :resize +1<CR>

" Make j and k move to next line, not a file line
" nnoremap j gj
" nnoremap k gk

nmap j <Plug>(accelerated_jk_gj)
nmap k <Plug>(accelerated_jk_gk)

" Prevent x from overriding what's in the clipboard
nnoremap x "_x
nnoremap X "_x

" Copy/Paste/Cut
nmap YY "+y<CR>
nmap P "+gP<CR>
nmap XX "+x<CR>

" Clean search (highlight)
nmap <silent> <F2> :<C-u>noh<CR>

" Prevent selecting and pasting from overwritting what you originally copied
xnoremap p pgvy

" Keep cursor at the bottom of the visual selection after you yank it
vmap y ygv<Esc>

" Maintain visual mode after shifting > and <
vmap < <gv
vmap > >gv

nmap >> >>_
nmap << <<_

" Saving read-only file
cmap w!! w !sudo tee % >/dev/null

" Use <> and <S-Tab> for navigate completion list
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Use <CR> to confirm completion
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Quit quickfix window by <Esc>"
augroup TYPESRC
	au FileType qf nnoremap <buffer><silent> <Esc> :quit<CR>

	" quit `fzf` window by <Esc>
	" au FileType fzf <buffer> nnoremap <silent> <Esc> <C-c>
augroup END

" Identify the syntax highlighting group under the cursor
map <f10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
  \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
  \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
