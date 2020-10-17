" --------------------
" SPACEMACS FOR NEOVIM
" --------------------
" Use FZF instead of denite
" Dependency: FZF

" Map leader to space
nmap <Space> <Nop>
let g:mapleader="\<Space>"
let g:maplocalleader="\<Space>"

" ------------------------
" UNIVERSAL SEARCH
" ------------------------
nnoremap <leader><Space> :<C-u>Files<CR>
nnoremap <leader>; :<C-u>RG<CR>
" ------------------------
" FILE & BUFFER NAVIGATION
" ------------------------

" Save
nnoremap <silent> <leader>fs :<C-u>w<CR>
nnoremap <silent> <leader>bs :<C-u>w<CR>

" Save all
nnoremap <silent> <leader>fS :<C-u>wa<CR>
nnoremap <silent> <leader>bS :<C-u>wa<CR>

" Close
" Smart way to close buffers without losing split windows
" See: http://bit.ly/2heyMZ8
nnoremap <silent> <leader>fd :<C-u>bp\|bd #<CR>
nnoremap <silent> <leader>fc :<C-u>bp\|bd #<CR>
nnoremap <silent> <leader>bd :<C-u>bp\|bd #<CR>
nnoremap <silent> <leader>bc :<C-u>bp\|bd #<CR>

" Create new file on current dir
nnoremap <leader>fo :<C-u>e <c-r>=expand("%:p:h") . "/"<CR>
nnoremap <leader>bo :<C-u>e <c-r>=expand("%:p:h") . "/"<CR>

" Rename file
nnoremap <leader>fr :<C-u>Rename<Space>
nnoremap <leader>br :<C-u>Rename<Space>

" Move file
nnoremap <leader>fm :<C-u>Move<Space>
nnoremap <leader>bm :<C-u>Move<Space>

" Delete file and its buffer
" Ref: http://bit.ly/2gxMMwc
" nnoremap <silent> <leader>fD :<C-u>call delete(expand('%')) | bdelete!
" nnoremap <silent> <leader>bD :<C-u>call delete(expand('%')) | bdelete!
nnoremap <leader>fD :<C-u>Delete!<CR>
nnoremap <leader>bD :<C-u>Delete!<CR>


" Quick open popular files
nnoremap <silent> <leader>fev :<C-u>e $MYVIMRC<CR>
nnoremap <silent> <leader>fez :<C-u>e ~/dotfiles/.zshrc<CR>

nnoremap <silent> <leader>ff :<C-u>Files<CR>
nnoremap <silent> <leader>fb :<C-u>Buffers<CR>
nnoremap <silent> <leader>bf :<C-u>Files<CR>
nnoremap <silent> <leader>bb :<C-u>Buffers<CR>

nnoremap <silent> <leader>fn :<C-u>bn<CR>
nnoremap <silent> <leader>fp :<C-u>bp<CR>
nnoremap <silent> <leader>bn :<C-u>bn<CR>
nnoremap <silent> <leader>bp :<C-u>bp<CR>


" ------------------------
" WINDOWS NAVIGATION
" ------------------------
nnoremap <silent> <leader>wj <C-w>j
nnoremap <silent> <leader>wk <C-w>k
nnoremap <silent> <leader>wh <C-w>h
nnoremap <silent> <leader>wl <C-w>l
nnoremap <silent> <leader>wJ <C-w>J
nnoremap <silent> <leader>wK <C-w>K
nnoremap <silent> <leader>wH <C-w>H
nnoremap <silent> <leader>wL <C-w>L
nnoremap <silent> <leader>wd <C-w>c
nnoremap <silent> <leader>wc <C-w>c
nnoremap <silent> <leader>ww <C-w>w
nnoremap <silent> <leader>w= <C-w>=
nnoremap <silent> <leader>ws :<C-u>sp<CR>
nnoremap <silent> <leader>wv :<C-u>vsplit<CR>

" Neovim Term only

tnoremap <silent> <leader>wh <C-\><C-n><C-w>h
tnoremap <silent> <leader>wj <C-\><C-n><C-w>j
tnoremap <silent> <leader>wk <C-\><C-n><C-w>k
tnoremap <silent> <leader>wl <C-\><C-n><C-w>l

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
nnoremap <silent> <leader>xj :m .+1<CR>==
nnoremap <silent> <leader>xk :m .-2<CR>==
inoremap <silent> <leader>xj <esc>:m .+1<CR>==gi
inoremap <silent> <leader>xk <esc>:m .-2<CR>==gi
vnoremap <silent> <leader>xj :m '>+1<CR>gv=gv
vnoremap <silent> <leader>xk :m '<-2<CR>gv=gv

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
nnoremap <leader>pp :<C-u>SLoad<Space>
nnoremap <leader>ps :<C-u>SSave<Space>

" ----------------------
" GIT
" ----------------------
" Use <leader>g
" nnoremap <silent> <leader>gd :Gdiff<CR>
" nnoremap <silent> <leader>gD :Gdiffoff<CR>
" nnoremap <silent> <leader>gc :Gcommit<CR>
" nnoremap <silent> <leader>gb :Gblame<CR>
" nnoremap <silent> <leader>gB :Gbrowse<CR>
" nnoremap <silent> <leader>gS :Gstatus<CR>
" nnoremap <silent> <leader>gp :Gpush<CR>

" ----------------------
" ERRORS HANDLING
" ----------------------
" Dependency: ALE

" Move between linting errors
" nnoremap <silent> <leader>en :<C-u>ALENextWrap<CR>
" nnoremap <silent> <leader>ep :<C-u>ALEPreviousWrap<CR>

" ----------------------
" TOGGLE
" ----------------------

nnoremap <silent> <leader>tr :<C-u>source $MYVIMRC<CR>:echo 'Neovim reloaded!'<CR>
" Neovim Terminal
nnoremap <silent> <leader>tt :<C-u>Ttoggle<CR>
tnoremap <silent> <leader>tt <C-\><C-n>:<C-u>Ttoggle<CR>

" Toggle Goyo
" nnoremap <silent> <leader>tg :<C-u>Goyo<CR>

" Indent guide
nnoremap <silent> <leader>ti :<C-u>IndentLinesToggle<CR>
" nnoremap <silent> <leader>tn :<C-u>!echo /dev/urandom \| base64 \| tr -dc 'a-zA-Z0-9' \| fold -w 7 \| head -n 1 \| pbcopy<CR>

" Markdown Preview
" nnoremap <silent> <leader>tM :<C-u>MarkdownPreview<CR>
nnoremap <silent> <leader>qq :<C-u>qa<CR>
nnoremap <silent> <leader>qQ :<C-u>qa!<CR>
