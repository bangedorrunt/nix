" WHICH-KEY
" ---------
" Use FZF instead of denite
" Dependency: FZF


" Map leader to space
nmap <Space> <Nop>
let g:mapleader="\<Space>"
let g:maplocalleader=","

call which_key#register('<Space>', "g:which_key_map")
nnoremap <silent> <leader> :<C-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :<C-u>WhichKeyVisual '<Space>'<CR>

" nnoremap <silent> <localleader> :<C-u>WhichKey  ','<CR>

let g:which_key_map = {}

" ------------------------
" UNIVERSAL SEARCH
" ------------------------
" Use <leader>
nnoremap <leader><Space> :<C-u>Files<CR>
nnoremap <leader>; :<C-u>RG<CR>

let g:which_key_map = {
      \ 'name': 'SPC',
      \ ' ':   ['Files', 'fzf-files'],
      \ ';':   ['RG', 'fzf-ripgrep']
      \}

" ------------------------
" FILE & BUFFER NAVIGATION
" ------------------------
" Use <leader>b
" Use <leader>f

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
nnoremap <leader>fr :<C-u>Rename<SPACE>
nnoremap <leader>br :<C-u>Rename<SPACE>

" Move file
nnoremap <leader>fm :<C-u>Move<SPACE>
nnoremap <leader>bm :<C-u>Move<SPACE>

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

let g:which_key_map.f = {
      \ 'name': '+files',
      \ 'f':    'fzf-files',
      \ 'b':    'fzf-buffers',
      \ 'n':    'next-buffer',
      \ 'p':    'previous-buffer',
      \ 'o':    'new-file',
      \ 'r':    'rename-file',
      \ 'd':    'delete-buffer',
      \ 'c':    'delete-buffer',
      \ 'm':    'move-file',
      \ 'D':    'delete-file',
      \ 's':    'save-file',
      \ 'S':    'save-all-files'
      \ }
let g:which_key_map.b = {
      \ 'name': '+buffers',
      \ 'f':    'fzf-files',
      \ 'b':    'fzf-buffers',
      \ 'n':    'next-buffer',
      \ 'p':    'previous-buffer',
      \ 'o':    'new-file',
      \ 'r':    'rename-file',
      \ 'd':    'delete-buffer',
      \ 'c':    'delete-buffer',
      \ 'm':    'move-file',
      \ 'D':    'delete-file',
      \ 's':    'save-file',
      \ 'S':    'save-all-files'
      \ }


" ------------------------
" CODE
" ------------------------
" Use <leader>c
let g:which_key_map.c = {
      \ 'name': '+code',
      \ 'p':    'prettier-format'       ,
      \ 'f':    ['LanguageClient#textDocument_formatting()',     'lsp-format'] ,
      \ 'h':    ['LanguageClient#textDocument_hover()',          'hover'] ,
      \ 'r':    ['LanguageClient#textDocument_references()',     'references'] ,
      \ 'R':    ['LanguageClient#textDocument_rename()',         'rename'] ,
      \ 's':    ['LanguageClient#textDocument_documentSymbol()', 'document- symbol']  ,
      \ 'S':    ['LanguageClient#workspace_symbol()',            'workspace- symbol'],
      \ 'd':    ['LanguageClient#textDocument_definition()',     'definition']       ,
      \ 't':    ['LanguageClient#textDocument_typeDefinition()', 'type- definition']  ,
      \ 'i':    ['LanguageClient#textDocument_implementation()', 'implementation']   ,
      \ }


" ----------------------
" GIT
" ----------------------
" Use <leader>g
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gc :Git commit<CR>
nnoremap <silent> <leader>gb :Git blame<CR>
nnoremap <silent> <leader>gB :GBrowse<CR>
nnoremap <silent> <leader>gs :Git<CR>
nnoremap <silent> <leader>gp :Git push<CR>

let g:which_key_map.g = {
      \ 'name': '+git',
      \ 's':    'git-status',
      \ 'c':    'git-commit',
      \ 'd':    'git-diff',
      \ 'D':    'git-diff-off',
      \ 'b':    'git-blame',
      \ 'B':    'git-browse',
      \ 'p':    'git-push'
      \ }
" ----------------------
" PROJECT MANAGER
" ----------------------
" User <leader>p
nnoremap <leader>pp :<C-u>SLoad<SPACE>
nnoremap <leader>ps :<C-u>SSave<SPACE>
let g:which_key_map.p = {
      \ 'name': '+project-manager',
      \ 'p':    'load-project',
      \ 's':    'save-project'
      \ }


" ----------------------
" TEXT MANIPULATION
" ----------------------
" Use <leader>x
" Move line up and down
" Ref: http://bit.ly/2epdrri
nnoremap <silent> <leader>xj :m .+1<CR>==
nnoremap <silent> <leader>xk :m .-2<CR>==
inoremap <silent> <leader>xj <Esc>:m .+1<CR>==gi
inoremap <silent> <leader>xk <Esc>:m .-2<CR>==gi
vnoremap <silent> <leader>xj :m '>+1<CR>gv=gv
vnoremap <silent> <leader>xk :m '<-2<CR>gv=gv

" Align text
xmap <silent> <leader>xa <Plug>(EasyAlign)
nmap <silent> <leader>xa <Plug>(EasyAlign)

let g:which_key_map.x = {
      \ 'name': '+edit-texts',
      \ 'j':    'move-line-down',
      \ 'k':    'move-line-up',
      \ 'a':    'align-texts'
      \ }

" ------------------------
" WINDOWS NAVIGATION
" ------------------------
" Use <leader>w
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

let g:which_key_map.w = {
      \ 'name': '+windows',
      \ 'h':    'window-left',
      \ 'j':    'window-below',
      \ 'l':    'window-right',
      \ 'k':    'window-up',
      \ 'H':    'move-window-left',
      \ 'J':    'move-window-below',
      \ 'L':    'move-window-right',
      \ 'K':    'move-window-up',
      \ 's':    'split-window-below',
      \ 'v':    'split-window-below',
      \ '=':    'balance-window',
      \ 'w':    'other-window',
      \ 'd':    'delete-window',
      \ 'c':    'delete-window'
      \ }

" ----------------------
" TOGGLE
" ----------------------
" Use <leader>t
nnoremap <silent> <leader>tr :<C-u>source $MYVIMRC<CR>:echo 'Neovim reloaded!'<CR>
" Neovim Terminal
nnoremap <silent> <leader>tt :<C-u>Ttoggle<CR>
tnoremap <silent> <leader>tt <C-\><C-n>:<C-u>Ttoggle<CR>

" Toggle Goyo
" nnoremap <silent> <leader>tg :<C-u>Goyo<CR>

" Indent guide
" Ref: indentline.vim
nnoremap <silent> <leader>ti :<C-u>IndentLinesToggle<CR>
" nnoremap <silent> <leader>tn :<C-u>!echo /dev/urandom \| base64 \| tr -dc 'a-zA-Z0-9' \| fold -w 7 \| head -n 1 \| pbcopy<CR>

" Markdown Preview
" nnoremap <silent> <leader>tM :<C-u>MarkdownPreview<CR>

let g:which_key_map.t = {
      \ 'name': '+toggle',
      \ 'r':    'reload-vim',
      \ 'i':    'toggle-indent',
      \ 's':    'toggle-fern-tree',
      \ 't':    'toggle-terminal'
      \ }

" ----------------------
" NEURON VIM
" ----------------------
" Use <leader>z
" Ref: zettel.vim
let g:which_key_map.z = {
      \ 'name': '+neuron',
      \ 'n':    'new-zettel',
      \ 'N':    'new-zettel-from-cword',
      \ 'r':    'refresh-neuron-cache',
      \ 'u':    'open-last-zettel',
      \ 'U':    'open-previous-history',
      \ 'P':    'open-next-history',
      \ 'z':    'find-all-zettels',
      \ 'o':    'open-zettel-link',
      \ 's':    'ripgrep-zettel',
      \ 'S':    'ripgrep-zettel-cword',
      \ 'l':    'insert-last-zettel',
      \ 'i':    'insert-selected-zettel',
      \ 'L':    'insert-last-history',
      \ 'I':    'insert-selected-history',
      \ 'v':    'toggle-backlinks'
      \ }

" ----------------------
" QUIT VIM
" ----------------------
nnoremap <silent> <leader>qq :<C-u>qa<CR>
nnoremap <silent> <leader>qQ :<C-u>qa!<CR>
let g:which_key_map.q = {
      \ 'name': '+quit',
      \ 'q':    'quit-all',
      \ 'Q':    'quit-all-without-saving'
      \ }



aug WHICH-KEY
  au! FileType which_key
  au  FileType which_key set laststatus=0 noshowmode noruler pb=0
   \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler pb=0
aug END
