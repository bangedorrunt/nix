" WHICH-KEY
" ---------

call which_key#register('<Space>', "g:which_key_map")

nnoremap <silent> <leader> :<c-u>WhichKey '<space>'<cr>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<space>'<cr>

let g:which_key_map = {}
let g:which_key_map = {
      \ 'name': 'SPC',
      \ ' ': ['Files', 'fzf-files'],
      \ ';': ['RG', 'fzf-ripgrep']
      \}

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

let g:which_key_map.l = {
      \ 'name' : '+lsp'                                            ,
      \ 'f' : ['LanguageClient#textDocument_formatting()'     , 'formatting'] ,
      \ 'h' : ['LanguageClient#textDocument_hover()'          , 'hover'] ,
      \ 'r' : ['LanguageClient#textDocument_references()'     , 'references'] ,
      \ 'R' : ['LanguageClient#textDocument_rename()'         , 'rename'] ,
      \ 's' : ['LanguageClient#textDocument_documentSymbol()' , 'document- symbol']  ,
      \ 'S' : ['LanguageClient#workspace_symbol()'            , 'workspace- symbol'] ,
      \ 'g' : {
      \ 'name': '+goto',
      \ 'd' : ['LanguageClient#textDocument_definition()'     , 'definition']       ,
      \ 't' : ['LanguageClient#textDocument_typeDefinition()' , 'type- definition']  ,
      \ 'i' : ['LanguageClient#textDocument_implementation()'  , 'implementation']   ,
          \ },
      \ }

let g:which_key_map.p = {
      \ 'name': '+project-manager',
      \ 'p':    'load-project',
      \ 's':    'save-project'
      \ }

let g:which_key_map.x = {
      \ 'name': '+edit-texts',
      \ 'j':    'move-line-down',
      \ 'k':    'move-line-up',
      \ 'a':    'align-texts'
      \ }

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

let g:which_key_map.t = {
      \ 'name': '+toggle',
      \ 'r':    'reload-vim',
      \ 'i':    'toggle-indent',
      \ 's':    'toggle-fern-tree',
      \ 't':    'toggle-terminal'
      \ }

let g:which_key_map.z = {
      \ 'name': '+zettel',
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
