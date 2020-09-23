" --------------------
" SPACEMACS FOR NEOVIM
" --------------------
" Use FZF instead of denite
" Dependency: FZF

" ------------------------
" UNIVERSAL SEARCH
" ------------------------
nnoremap <silent> <leader><space> :<c-u>Files<cr>
nnoremap <leader>; :<c-u>RG<cr>
" ------------------------
" FILE & BUFFER NAVIGATION
" ------------------------

" Save
nnoremap <silent> <leader>fs :<c-u>w<cr>
nnoremap <silent> <leader>bs :<c-u>w<cr>

" Save all
nnoremap <silent> <leader>fS :<c-u>wa<cr>
nnoremap <silent> <leader>bS :<c-u>wa<cr>

" Close
" Smart way to close buffers without losing split windows
" See: http://bit.ly/2heyMZ8
nnoremap <silent> <leader>fd :<c-u>bp\|bd #<cr>
nnoremap <silent> <leader>fc :<c-u>bp\|bd #<cr>
nnoremap <silent> <leader>bd :<c-u>bp\|bd #<cr>
nnoremap <silent> <leader>bc :<c-u>bp\|bd #<cr>

" Create new file on current dir
nnoremap <leader>fo :<c-u>e <c-r>=expand("%:p:h") . "/"<cr>
nnoremap <leader>bo :<c-u>e <c-r>=expand("%:p:h") . "/"<cr>

" Rename file
nnoremap <leader>fr :<c-u>Rename<space>
nnoremap <leader>br :<c-u>Rename<space>

" Move file
nnoremap <leader>fm :<c-u>Move<space>
nnoremap <leader>bm :<c-u>Move<space>

" Delete file and its buffer
" Ref: http://bit.ly/2gxMMwc
" nnoremap <silent> <leader>fD :<c-u>call delete(expand('%')) | bdelete!
" nnoremap <silent> <leader>bD :<c-u>call delete(expand('%')) | bdelete!
nnoremap <silent> <leader>fD :<c-u>Delete<cr>
nnoremap <silent> <leader>bD :<c-u>Delete<cr>

" Quick open popular files
nnoremap <silent> <leader>fev :<c-u>e $MYVIMRC<cr>
nnoremap <silent> <leader>fez :<c-u>e ~/dotfiles/.zshrc<cr>

nnoremap <silent> <leader>ff :<c-u>Files<cr>
nnoremap <silent> <leader>bf :<c-u>Buffers<cr>
nnoremap <silent> <leader>bb :<c-u>Buffers<cr>

nnoremap <silent> <leader>fn :<c-u>bn<cr>
nnoremap <silent> <leader>fp :<c-u>bp<cr>
nnoremap <silent> <leader>bn :<c-u>bn<cr>
nnoremap <silent> <leader>bp :<c-u>bp<cr>


" ------------------------
" WINDOWS NAVIGATION
" ------------------------
nnoremap <silent> <leader>wj <c-w>j
nnoremap <silent> <leader>wk <c-w>k
nnoremap <silent> <leader>wh <c-w>h
nnoremap <silent> <leader>wl <c-w>l
nnoremap <silent> <leader>wJ <c-w>J
nnoremap <silent> <leader>wK <c-w>K
nnoremap <silent> <leader>wH <c-w>H
nnoremap <silent> <leader>wL <c-w>L
nnoremap <silent> <leader>wd <c-w>c
nnoremap <silent> <leader>wc <c-w>c
nnoremap <silent> <leader>w= <c-w>=
nnoremap <silent> <leader>ws :<c-u>sp<cr>
nnoremap <silent> <leader>wv :<c-u>vsplit<cr>

" Neovim Term only

tnoremap <silent> <leader>wh <c-\><c-n><c-w>h
tnoremap <silent> <leader>wj <c-\><c-n><c-w>j
tnoremap <silent> <leader>wk <c-\><c-n><c-w>k
tnoremap <silent> <leader>wl <c-\><c-n><c-w>l

" ----------------------
" REGISTER
" ----------------------
" Use <leader>r

" ----------------------
" TEXT MANIPULATION
" ----------------------
" Use <leader>x

" Move line up and down
" Ref: http://bit.ly/2epdrri
nnoremap <silent> <leader>xj :m .+1<cr>==
nnoremap <silent> <leader>xk :m .-2<cr>==
inoremap <silent> <leader>xj <esc>:m .+1<cr>==gi
inoremap <silent> <leader>xk <esc>:m .-2<cr>==gi
vnoremap <silent> <leader>xj :m '>+1<cr>gv=gv
vnoremap <silent> <leader>xk :m '<-2<cr>gv=gv

" Align text
xmap <silent> <leader>xa <plug>(EasyAlign)
nmap <silent> <leader>xa <plug>(EasyAlign)

" ----------------------
"  JUMP
" ----------------------
" nmap <silent> <leader>j <plug>(easymotion-bd-jk)
" nnoremap <silent> <leader>j <plug>(easymotion-j)
" nnoremap <silent> <leader>k <plug>(easymotion-k)

" ----------------------
" PROJECT MANAGER
" ----------------------
" Use <leader>p
nnoremap <leader>pp :<c-u>SLoad<space>
nnoremap <leader>ps :<c-u>SSave<space>

" ----------------------
" GIT
" ----------------------
" Use <leader>g
" nnoremap <silent> <leader>gd :Gdiff<cr>
" nnoremap <silent> <leader>gD :Gdiffoff<cr>
" nnoremap <silent> <leader>gc :Gcommit<cr>
" nnoremap <silent> <leader>gb :Gblame<cr>
" nnoremap <silent> <leader>gB :Gbrowse<cr>
" nnoremap <silent> <leader>gS :Gstatus<cr>
" nnoremap <silent> <leader>gp :Gpush<cr>

" ----------------------
" ERRORS HANDLING
" ----------------------
" Dependency: ALE

" Move between linting errors
" nnoremap <silent> <leader>en :<c-u>ALENextWrap<cr>
" nnoremap <silent> <leader>ep :<c-u>ALEPreviousWrap<cr>

" ----------------------
" TOGGLE
" ----------------------

nnoremap <silent> <leader>tr :<c-u>source $MYVIMRC<cr>:echo 'Neovim reloaded!'<cr>
" Neovim Terminal
nnoremap <silent> <leader>tt :<c-u>Ttoggle<cr>
tnoremap <silent> <leader>tt <c-\><c-n>:<c-u>Ttoggle<cr>

" Toggle Goyo
" nnoremap <silent> <leader>tg :<c-u>Goyo<cr>

" Indent guide
nnoremap <silent> <leader>ti :<c-u>IndentLinesToggle<cr>
nnoremap <silent> <leader>tn :<c-u>!echo /dev/urandom \| base64 \| tr -dc 'a-zA-Z0-9' \| fold -w 7 \| head -n 1 \| pbcopy<cr>

" Markdown Preview
" nnoremap <silent> <leader>tM :<c-u>MarkdownPreview<cr>

